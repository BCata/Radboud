function k = kernel( x, xt, theta )

k = theta(1).*exp(-(theta(2)./2).*sum((x-xt).^2))+theta(3)+theta(4).*(x'*xt);

end