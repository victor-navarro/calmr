% Simulate blocking experiment.
clearvars; clc; 
rng(2) % Use the same seed

% Set task parameters
meanITI = 30;
maxITI = meanITI*3;
cuerewdelay = 1; 
numcs1only = 50;
numcs1cs2 = 50;

% Model parameters
samplingperiod = 0.2;
alpha = 0.02;
alpha_r = 10*alpha;
w = 0.5;
k = 0.01;
Tratio = 1.2;
T = meanITI*Tratio;
minimumrate = 10^(-3);
threshold = 0.6;
maximumjitter = 0.1;
beta = [0, 0, 1]';
exact_mean_or_not = 0;
nIter = 1;
% Set decay time constant

%% Simulate

% Pre-blocking - generate eventlog with only CS1->R
eventlog_preblock = simulateEvents(numcs1only, 1, 3, 1, nan,...
    meanITI, meanITI*3, cuerewdelay, 1, 0);

% Generate eventlog for CS1+CS2 pairings
eventlog_paired = simulateEventChain(numcs1cs2, 2, nan, meanITI, ...
    meanITI*3, 0, cuerewdelay, 1, 0);

% Shift eventlog for CS1+CS2 pairings by the last timestamp from 
% eventlog_preblock
eventlog_paired(:,2) = eventlog_paired(:,2) + eventlog_preblock(end,2);
% Concatenate preblock and paired eventlogs
eventlog = [eventlog_preblock; eventlog_paired];

[DA,ANCCR,PRC,SRC,NC,Rs,Delta,Mij,Mi,Eij,Ei,eventlog] = calculateANCCR_v3(eventlog, T, ...
    alpha, k, samplingperiod, w,threshold,minimumrate,beta, ...
    alpha_r, maximumjitter, nan, nan);

% Estimate value with and without inhibition
q_src = SRC.*Rs;

%% Save data
save("C:/Users/Victor/calmr/anccr_tests/simple_blocking.mat")
