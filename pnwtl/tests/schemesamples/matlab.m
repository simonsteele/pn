% Example Monte Carlo Simulation in Matlab
% Function: y = x2^2/x1
%
%   Generate n samples from a normal distribution
%     r = ( randn(n,1) * sd ) + mu
%     mu : mean
%     sd : standard deviation
%
%   Generate n samples from a uniform distribution
%     r = a + rand(n,1) * (b-a)
%     a : minimum
%     b : maximum

n = 100000;	% The number of function evaluations

% --- Generate vectors of random inputs
% x1 ~ Normal distribution N(mean=100,sd=5)
% x2 ~ Uniform distribution U(a=5,b=15)

x1 = ( randn(n,1) * 5 ) + 100;
x2 = 5 + rand(n,1) * ( 15 - 5 );

% --- Run the simulation
% Note the use of element-wise multiplication

y = x2.^2 ./ x1;

% --- Create a histogram of the results (50 bins)

hist(y,50);

% --- Calculate summary statistics

y_mean = mean(y)
y_std = std(y)
y_median = median(y)