function p = bernll( x, mu )

p = 1;
D = length(x);

for i=1:D
    fst = mu(i).^x(i);
    snd = (1-mu(i)).^(1-x(i));
    p = p.*fst.*snd;
end

