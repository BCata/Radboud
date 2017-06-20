%probs is NxK matrix where N is number of data set points and K is
%number of classes. Every row of this matrix represents probabilities
%of assigning given point to class represented by column.
probs = zeros(N,K);
for k=1:K
    probs(:,k) = pi(k).*mvnpdf(X, mu(k,:), sigma(:,:,k));
end
[M, I] = max(probs');

%scale I
a = 0; b = 1;
mn = min(I); mx = max(I);

mycolormap = colormap('Jet');
inp = 0:63;
d64 = inp./63;
I = ((b-a).*(I-mn))./(mx-mn) + a;
c = interp1(d64, mycolormap, I);

dotsize = 20;
sca = scatter(X(:,1), X(:,2), dotsize,c,'fill');
colorbar;
xlabel('x1'); ylabel('x2');