% some ideas to make a scatter plot in the logistic regression exercise
% you may use and adapt this code according to your needs 

% just a random set as an example
%x = randn(1000,2);
% "class probabilities"
%cl = 1./(1+exp(-(w'*phi)));
cl = t;

% this seems to work
mycolormap = colormap('autumn');
inp = [0:63];
d64 = inp./63;
c = interp1(d64, mycolormap, cl);
dotsize = 10;
sca = scatter(phi(2,:),phi(3,:),dotsize,c,'fill');
xlabel('x_1');
ylabel('x_2');
title('Data scatterplot');
colorbar; % what do the colors mean?