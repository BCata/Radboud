function [ logllh ] = logLh( data )

a = 0;
b = 1;

cons = size(data,2);
logllh = cons
for i = 1:cons
   logllh = logllh - log((data(1,i)-a).^2 + b.^2); 
end

end

