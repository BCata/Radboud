
initWeights1 = rand([3 40])-0.5;
initWeights2 = rand([41 1])-0.5;

%mu = [0 0];
%sigma = [0.4 0; 0 0.4];
%x1 = -2:.1:2;
%x2 = -2:.1:2;
%[X1, X2] = meshgrid(x1, x2);
%X = [X1(:) X2(:)];
%Y = 3*mvnpdf(X, mu, sigma);
%y = reshape(Y, length(x1), length(x2));

Y_init = myNN(X, initWeights1, initWeights2);
y_init = reshape(Y_init, length(x1), length(x2));

surf(x1, x2, y_init);
xlabel('x1'); ylabel('x2'); zlabel('y');
fprintf('Initial output\n');
fprintf('Press enter to continue\n');
pause;

w1 = initWeights1;
w2 = initWeights2;

for i=1:2000
   [w1, w2] = trainNeuralNet(X, Y, w1, w2);
   
   if mod(i, 200) == 0
       OUT = myNN(X, w1, w2);
       out = reshape(OUT, length(x1), length(x2));
       surf(x1, x2, out);
       xlabel('x1'); ylabel('x2'); zlabel('y'); 
       fprintf('Output after %d training cycles\n', i);
       pause;
   end
end