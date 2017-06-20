N = length(X);
t = C;
%w = [0.0044 ; -0.0214 ; -0.0493];
%w = [0 ; 0 ; 0];

E = 0;
y = sigmoid((w'*phi)');

for i=1:N
    E = E - (t(i,1)*log(y(i,1)) + (1-t(i,1))*log(1-y(i,1)));
end
E
