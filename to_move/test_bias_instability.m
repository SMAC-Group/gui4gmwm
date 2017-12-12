%%
format long g
clear all
clc

%%
B = 4;
f0 = 1;

T = logspace(-2,2,100);
x = pi*f0*T;

sigma2 = @(x) 2 * B^2 / pi * ( ...
                            log(2) - (sin(x)^3 / (2*x^2)) * (sin(x) + 4*x*cos(x)) + cosint(2*x) - cosint(4*x) ...
                        );

for i = 1:1:length(T)
    sigma2_T(i) = sigma2(x(i));
end
clear i

figure(1)
clf
loglog(T, sqrt(sigma2_T), '-s')
grid on
 


