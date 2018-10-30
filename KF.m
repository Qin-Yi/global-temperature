A=[1,1;0,1]
B=[1,0;0,1]
C=[1,0]
D=1
y= No_Smoothing
Mdl = ssm(A,B,C,D)

A=NaN;B=NaN;C=1;D=NaN;
StateType = 0;
Mdl = ssm(A,B,C,D,'StateType',StateType);
params0 = [1,1,1];
EstMdl = estimate(Mdl,y,params0,'lb',[-Inf,0,0]);
Output = refine(Mdl,y,params0);
logL = cell2mat({Output.LogLikelihood})';
[~,maxLogLIndx] = max(logL)
refinedParams0 = Output(maxLogLIndx).Parameters
Description = Output(maxLogLIndx).Description
EstMdl = estimate(Mdl,y,refinedParams0,'lb',[-Inf,0,0]);

filteredX = filter(EstMdl,y);
figure
plot(Year,y,'-k',Year,filteredX,':r','LineWidth',2)
title({'State Values'})
xlabel('Period')
ylabel('State')
legend({'True state values','Filtered state values'})