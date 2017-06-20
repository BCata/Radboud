function [ ] = lighthousePlot14( )

D = [4.8 -2.7 2.2 1.1 0.8 -7.3];
%D = [-1 0 1 -2 2];
N = 100000;
y = zeros(1,N);
xMesh = linspace(-5,5,N);
dSize = 6;

beta = 1;

for n = 1:N
    a = xMesh(n);
    list = zeros(1,dSize);
    for i = 1:dSize
        x = D(1,i);
        list(1,i) = beta./(pi.*((x-a).*(x-a)+beta.*beta));
    end
    y(1,n) = prod(list);
end

plot(xMesh, y)

end

