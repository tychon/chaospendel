
printf("reading in.csv ...\n");
A = csvread("in.csv");
printf("plotting ...\n");
plot (A, "-;input;");
print ("-dpng", "plot_input.png");


printf("reading out.csv ...\n");
A = csvread("out.csv");

printf("plotting 0-10...\n");
xstart = 0;
xend = min(10, length(A(:,1))-1);
X = (xstart : xend);
plot (X, A(1:(xend-xstart)+1,1), ".-;real;", X, A(1:(xend-xstart)+1,2), ".-;imag;");
print ("-dpng", "plot_fourier0-10.png");

printf("plotting 0-20...\n");
xstart = 0;
xend = min(20, length(A(:,1))-1);
X = (xstart : xend);
plot (X, A(1:(xend-xstart)+1,1), ".-;real;", X, A(1:(xend-xstart)+1,2), ".-;imag;");
print ("-dpng", "plot_fourier0-20.png");

printf("plotting all...\n");
plot (A(:,1), ".-;real;", A(:,2), ".-;imag;");
print ("-dpng", "plot_fourierAll.png");

