function [  ] = plotLikelihood( data, maxK )

[A, B] = meshgrid(linspace(-10, 10, 200), linspace(0, 5, 50));
k = [1 2 3 20];
for i = 1:maxK
   const = maxK.*log((1./pi).*B);
   logllh = const;
   
   for n = 1:i
       logllh = logllh - log((data(1,n) - A).^2 + B.^2);
   end
   
    surf(A,B,logllh);
    fminsearch(logllh, [1 0])
end

end

