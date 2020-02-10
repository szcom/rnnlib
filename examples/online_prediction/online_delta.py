#!/usr/bin/env python

import re
import os
from optparse import OptionParser
from xml.dom.minidom import parse

import netcdf_helpers
from scipy import *


def Std(array, axis):
    if shape(array)[axis] > 1:
        return std(array, axis)
    return array


def get_target_string(stroke_file_name):
    ascii_file_name = re.sub('lineStrokes', 'ascii', stroke_file_name)
    ascii_file_name = re.sub('-[0-9]+\.xml', '.txt', ascii_file_name)
    try:
        line_nr = int(re.search('-([0-9]+)\.xml', stroke_file_name).group(1))
        with open(ascii_file_name, 'r') as ascii_file:
            lines = [line.strip() for line in ascii_file]
        return lines[line_nr + lines.index('CSR:') + 1]
    except (AttributeError, IndexError):
        raise SystemExit


def write_to_file(filename, inputs, pred_seq_lengths, predictions, seq_dims, seq_lengths, seq_tags, target_seq_dims,
                  target_strings, word_target_strings):

    out_file = netcdf_helpers.netcdf_file(filename, 'w')

    # create the dimensions
    netcdf_helpers.create_nc_dim(out_file, 'numSeqs', len(seq_lengths))
    netcdf_helpers.create_nc_dim(out_file, 'numTimesteps', len(inputs))
    netcdf_helpers.create_nc_dim(out_file, 'predNumTimesteps', len(predictions))
    netcdf_helpers.create_nc_dim(out_file, 'inputPattSize', len(inputs[0]))
    netcdf_helpers.create_nc_dim(out_file, 'numDims', 1)

    # create the variables
    netcdf_helpers.create_nc_strings(out_file, 'seqTags', seq_tags, ('numSeqs', 'maxSeqTagLength'), 'sequence tags')
    netcdf_helpers.create_nc_strings(out_file, 'targetStrings', target_strings, ('numSeqs', 'maxTargStringLength'),
                                     'target strings')
    netcdf_helpers.create_nc_strings(out_file, 'wordTargetStrings', word_target_strings,
                                     ('numSeqs', 'maxWordTargStringLength'),
                                     'word target strings')
    netcdf_helpers.create_nc_var(out_file, 'seqLengths', seq_lengths, 'i', ('numSeqs',), 'sequence lengths')
    netcdf_helpers.create_nc_var(out_file, 'seqDims', seq_dims, 'i', ('numSeqs', 'numDims'), 'sequence dimensions')
    netcdf_helpers.create_nc_var(out_file, 'inputs', inputs, 'f', ('numTimesteps', 'inputPattSize'), 'input patterns')
    netcdf_helpers.create_nc_var(out_file, 'predSeqLengths', pred_seq_lengths, 'i', ('numSeqs',),
                                 'pred sequence lengths')
    netcdf_helpers.create_nc_var(out_file, 'targetSeqDims', target_seq_dims, 'i', ('numSeqs', 'numDims'),
                                 'pred sequence dimensions')
    netcdf_helpers.create_nc_var(out_file, 'targetPatterns', predictions, 'f', ('predNumTimesteps', 'inputPattSize'),
                                 'prediction patterns')
    out_file.close()


def process_lines(lines, tmp_filename):
    seq_dims = []
    seq_lengths = []
    target_strings = []
    word_target_strings = []
    seq_tags = []
    inputs = []
    predictions = []
    pred_seq_lengths = []
    target_seq_dims = []

    for line in lines:
        inkmlfile = line.strip()
        if len(inkmlfile):
            seq_tags.append(inkmlfile)
            word_target_strings.append(' ')
            seq_txt = get_target_string(inkmlfile)
            target_strings.append(seq_txt)
            old_len = len(inputs)
            old_len_pred = len(predictions)
            first_coord = array([])
            for trace in parse(inkmlfile).getElementsByTagName('Stroke'):
                for coords in trace.getElementsByTagName('Point'):
                    pt = array([float(coords.getAttribute('x').strip()), float(coords.getAttribute('y').strip())])
                    last = array([float(pt[0]), float(pt[1]), 0.0])
                    if len(first_coord) == 0:
                        first_coord = last
                    last = last - first_coord
                    inputs.append(last)
                inputs[-1][-1] = 1
            predictions.extend(inputs[old_len + 1:])
            predictions.append([float(0.0), float(0.0), float(0.0)])
            seq_lengths.append(len(inputs) - old_len)
            pred_seq_lengths.append(len(predictions) - old_len_pred)
            seq_dims.append([seq_lengths[-1]])
            target_seq_dims.append([pred_seq_lengths[-1]])

    write_to_file(tmp_filename, inputs, pred_seq_lengths, predictions, seq_dims, seq_lengths, seq_tags, target_seq_dims,
                  target_strings, word_target_strings)


def merge_tmp_files(tmp_files, nc_filename):
    print("Merging...")

    seq_dims = []
    seq_lengths = []
    target_strings = []
    word_target_strings = []
    seq_tags = []
    inputs = []
    target_patterns = []
    pred_seq_lengths = []
    target_seq_dims = []

    for tmp_filename in tmp_files:
        tmp_file = netcdf_helpers.netcdf_file(tmp_filename, 'r')

        seq_dims.extend(tmp_file.variables['seqDims'][:])
        seq_lengths.extend(tmp_file.variables['seqLengths'][:])
        target_strings.extend(tmp_file.variables['targetStrings'][:])
        word_target_strings.extend(tmp_file.variables['wordTargetStrings'][:])
        seq_tags.extend(tmp_file.variables['seqTags'][:])
        inputs.extend(tmp_file.variables['inputs'][:])
        target_patterns.extend(tmp_file.variables['targetPatterns'][:])
        pred_seq_lengths.extend(tmp_file.variables['predSeqLengths'][:])
        target_seq_dims.extend(tmp_file.variables['targetSeqDims'][:])

        tmp_file.close()
        os.remove(tmp_filename)

    first_ix = 0
    for i in range(len(seq_lengths)):
        for k in reversed(range(seq_lengths[i])):
            if k > 0:
                inputs[first_ix + k] = array(inputs[first_ix + k]) - array(inputs[first_ix + k - 1])
                inputs[first_ix + k][-1] = abs(inputs[first_ix + k][-1])
                target_patterns[first_ix + k - 1] = inputs[first_ix + k]
            if k == 0:
                target_patterns[first_ix] = inputs[first_ix + 1]
        inputs[first_ix] = array([0, 0, 0])
        first_ix += seq_lengths[i]

    write_to_file(nc_filename, inputs, pred_seq_lengths, target_patterns, seq_dims, seq_lengths, seq_tags,
                  target_seq_dims, target_strings, word_target_strings)


def main():
    # command line options
    parser = OptionParser()

    # parse command line options
    (options, args) = parser.parse_args()
    if len(args) < 2:
        print "usage: -options input_filename output_filename"
        print options
        sys.exit(2)

    input_filename = args[0]
    nc_filename = args[1]

    tmp_files = []
    all_lines = file(input_filename).readlines()
    batch_size = 500
    for i in range(0, len(all_lines), batch_size):
        print("Batch", str(i + 1), "Batch size:", str(batch_size))
        tmp_filename = 'tmp_' + str(int(i / batch_size)) + '.nc'
        tmp_files.append(tmp_filename)
        line_batch = all_lines[i:i + batch_size]
        process_lines(line_batch, tmp_filename)

    merge_tmp_files(tmp_files, nc_filename)


if __name__ == '__main__':
    main()
