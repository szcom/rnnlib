function [] = show_pen(fileName)
%SHOW_PEN plots the pen trajectory
%   SHOW_PEN(FILE) plots pen movements step by step; each step is encoded as 
%   dx,dy,end-of-stroke tuple; the FILE contains list of these tuples defining
%   pen movements to show

fid = fopen(fileName);
C = textscan(fid, '%s','delimiter', '\n');
fclose(fid);
index =  regexp(C{:}, '^Sample');
firstLine = find(~cellfun(@isempty,index))(1);
firstLine -= 1;
[b dx dy end_of_stroke] = textread (fileName, '%s %f %f %f', 'headerlines', firstLine, 'delimeter', '\n');
dx(isnan(dx))=0;
dy(isnan(dy))=0;
end_of_stroke(isnan(end_of_stroke))=0;

muXY =   [ 8.16576672  0.11146504];
devXY =  [ 41.484401  36.86198807];
m = size(dx(:), 1)

%set x(t) = x(t-1) + x(t) and y(t) = y(t-1) + y(t)
x = ( dx(:) * devXY(1) + muXY(1)  );
x = x' * triu(ones(m,m));
y = ( dy(:) * devXY(2) + muXY(2)  );
y = y' * triu(ones(m,m));

x = x(:);
y = y(:);
end_of_stroke = end_of_stroke(:);

figure(1);
clf;
hold on;
lift_offs = find(end_of_stroke == 1);
lift_offs = [lift_offs; size(end_of_stroke,1)];
lasti = 1;
for i=1:size(lift_offs,1)
       g = lasti:lift_offs(i);
       plot(x(g), -y(g));
       lasti = lift_offs(i) + 1;
    end;
end;
hold off;
