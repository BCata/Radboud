N = 800; D = 28*28; X = zeros(N,D);
d = 28;
fid = fopen('a012_images.dat', 'r');
fid2 = fopen('a012_labels.dat', 'r');
Z = fread(fid2, N);
img = zeros(d, d, N);
for i=1:N
   X(i,:) = fread(fid, D);
   img(:,:,i) = reshape(X(i,:), d, d);
end
status = fclose(fid);
status2 = fclose(fid2);