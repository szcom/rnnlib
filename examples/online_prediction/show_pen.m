function [] = show_pen(fileName)
fid = fopen(fileName);
C = textscan(fid, '%s','delimiter', '\n');
fclose(fid);
index =  regexp(C{:}, '^Sample');
firstLine = find(~cellfun(@isempty,index))(1);
firstLine -= 1;
[b xi2 ti2 b2] = textread (fileName, '%s %f %f %f', 'headerlines', firstLine, 'delimeter', '\n');

muXY =   [ 8.16576672  0.11146504];
devXY =  [ 41.484401  36.86198807];

xi2(:) = xi2(:)*devXY(1) + muXY(1);
ti2(:) = ti2(:)*devXY(2) + muXY(2);

for i=2:size(xi2,1) xi2(i) = xi2(i-1) + xi2(i); ti2(i) = ti2(i-1) + ti2(i); end;

figure(1);
clf;
hold on;
lasti = 1;
for i=1:size(b2,1)
    if b2(i) > 0 || i == size(b2,1)
       g = lasti:i;
       plot(xi2(g), -ti2(g));
       lasti = i + 1;
    end;
end;
hold off;

