% simulate net contingency with multiple number of associations
% c1->r1
% c2->r2
% c3->c4->r3
% c5,c6,c7,c9: not associated with reward

clearvars; clc; close all;
rng(2);

%% parameter set up
% task parameters
numcue = 30;
cuerewdelay = [2,2,4,0,0,0,0]; %c1,2,3,5,6,7,8
cuecuedelay = 2;
meanITI = 30;
rew_probs = [1,1,1,0,0,0,0];
IRI = meanITI/3;

% anccr model parameters
samplingperiod = 0.2;   
alpha = 0.02;        
alpha_r = 0.2;
w = 0.5;               
k = 1;                 
minimumrate = 10^(-3);
maximumjitter = 0.1;
beta = ones(1,11); % assumed all stimuli are MCTs to get net contingency across evey possible pairs
threshold = 0.6;
Tratio = 1.2;
T = IRI*Tratio;
exact_mean_or_not = 1;


%%

%generate eventlog
eventlog = simulateEventsTrialLess(repmat(numcue,1,7),[1,2,3,5,6,7,8],[9,10,11,nan,nan,nan,nan],...
    [1,1,1,nan,nan,nan,nan],nan,meanITI,meanITI*3,0,cuerewdelay,rew_probs);
cs3idx = eventlog(:,1)==3;
eventlog_c4 = [ones(sum(cs3idx),1)*4,eventlog(cs3idx,2)+cuecuedelay,zeros(sum(cs3idx),1)];
eventlog = [eventlog;eventlog_c4];
eventlog = sortrows(eventlog,2);

% simulate anccr

[DA,ANCCR,PRC,SRC,NC,Rs,Delta,Mij,Mi,Eij,Ei,eventlog] = calculateANCCR_v3(eventlog, T, ...
    alpha, k, samplingperiod, w,threshold,minimumrate,beta, ...
    alpha_r, maximumjitter, nan, nan,exact_mean_or_not);

% Estimate value
q_src = SRC.*Rs;

%% Save data
save("C:/Users/Victor/calmr/anccr_tests/multiple_associations.mat")

