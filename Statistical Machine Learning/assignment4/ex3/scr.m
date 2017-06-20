X = load('a011_mixdata.txt', '-ASCII');
[N, D] = size(X);

%{
hold on
histogram(X(:,1));
histogram(X(:,2));
histogram(X(:,3));
histogram(X(:,4));
xlabel('value');
ylabel('frequency');
legend('x1', 'x2', 'x3', 'x4');
%}

dep = X(:,4);
mycolormap = colormap('Jet');
inp = [0:63];
a = min(dep);
b = max(dep);
mn = 0;
mx = 1;
d64 = inp./63;
dScaled = ((b-a).*(d64-mn))./(mx-mn) + a;
c = interp1(dScaled, mycolormap, dep);
dotsize = 10;
sca = scatter3(X(:,1), X(:,2), X(:,3),dotsize,c,'fill');
colorbar;
caxis([a b]);
xlabel('x1'); ylabel('x2'); zlabel('x3');
