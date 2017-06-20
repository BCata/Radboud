function [ data, mean ] = generateData( a, b , N )

data = zeros(1, N);
mean = zeros(1,N);
currMean = 0;

for n = 1:N
    angle = rand(1).*pi - (pi./2);
    value = b.*tan(angle) + a;
    data(1,n) = value;
    mean(1,n) = currMean + (1./n) .* (value-currMean);
    currMean = mean(1,n);
end

end

