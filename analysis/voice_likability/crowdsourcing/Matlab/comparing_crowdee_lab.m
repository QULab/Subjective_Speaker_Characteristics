

clear; clc



% Paths

path_results='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\21-crowdeescale\csv-Crowdee\Final';
path_matlab='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\21-crowdeescale\Matlab';
path_json='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\21-crowdeescale\json_development\list8_prod';
path_11='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\11\matlab';
path_figures=[path_matlab,'\Figures'];
path_utils='D:\Users\fernandez.laura\Documents\Work\utils\Matlab\Plots'; % for rotateXLabels

addpath(path_matlab)
addpath(path_utils)




% Load the saved crowdee and laboratory results

load([path_matlab,'\Lab_results_males.mat'])
load([path_matlab,'\Lab_results_females.mat'])

crowdee_m=load([path_matlab,'\crowdee_results_Answers_Job_191_2017-02-06T09-26-07.946Z.mat']); % males
crowdee_f=load([path_matlab,'\crowdee_results_Answers_Job_190_2017-02-06T09-26-10.345Z.mat']); % females


%% Sort from most to least liked keeping trak of the pseudonym

% Sort males

[meanLab_males_sorted, indexes_m]=sort(meanLab_males,'descend');
[meanLab_females_sorted, indexes_f]=sort(meanLab_females,'descend');

meanLab_males=meanLab_males_sorted;
meanLab_females=meanLab_females_sorted;

meanCrowdee_males=crowdee_m.meanRating(indexes_m);
meanCrowdee_females=crowdee_f.meanRating(indexes_f);




%% Compute correlation and standard error

meanCrowdee=[meanCrowdee_males; meanCrowdee_females];
meanLab=[meanLab_males; meanLab_females];

pearsonr = corr(meanCrowdee,meanLab)
se = sqrt((1-pearsonr^2)/(length(meanCrowdee)-2))
% formula se from https://people.richland.edu/james/lecture/m170/ch11-cor.html

% males only
pearsonr=corr(meanCrowdee_males, meanLab_males) % [meanCrowdee_males, meanLab_males]
se = sqrt((1-pearsonr^2)/(length(meanLab_males)-2))

% females only
pearsonr=corr(meanCrowdee_females, meanLab_females)
se = sqrt((1-pearsonr^2)/(length(meanLab_females)-2))



% for all responses (+ 7 rejected workers):
% both genders: r=0.78, se= 0.12
% male spk only: r=0.71, se= 0.20
% female spk only: r=0.88, se= 0.13


% for only accepted responses (30 to each utt):
% both genders: r=0.78, se= 0.12
% male spk only: r=0.68, se= 0.20
% female spk only: r=0.89, se= 0.13





%% Plot means all speakers

% All speakers

ciLab=[ciLab_males(indexes_m);ciLab_females(indexes_f)];
ciCrowdee=[crowdee_m.cimeanRating(indexes_m);crowdee_f.cimeanRating(indexes_f)];


% namePlot='likabilityscale_lab_crowdee_all';
namePlot='likabilityscale_lab_crowdee_all_NoPseudonyms';
fig=figure('DefaultAxesFontSize',18);
skip=0.05;

colorlab = [43,140,190]/255; 
colorcrowdee = [240,59,32]/255;

xaxis_m = 1:15;
xaxis_f = 18:32;
xaxis_blank = 16:17;

h1=plot((xaxis_m)-skip, meanLab(1:15), '-sb','MarkerSize',6,'Color',colorlab,'MarkerFaceColor',colorlab,'MarkerEdgeColor','k');
hold on
h2=plot((xaxis_f)-skip, meanLab(16:30), '-sb','MarkerSize',6,'Color',colorlab,'MarkerFaceColor',colorlab,'MarkerEdgeColor','k');
hold on
errorbar((xaxis_m)-skip, meanLab(1:15),meanLab(1:15)-ciLab(1:15,1), '-sb','MarkerSize',6,'Color',colorlab,'MarkerFaceColor',colorlab,'MarkerEdgeColor','k')
hold on
errorbar((xaxis_f)-skip, meanLab(16:30),meanLab(16:30)-ciLab(16:30,1), '-sb','MarkerSize',6,'Color',colorlab,'MarkerFaceColor',colorlab,'MarkerEdgeColor','k')
hold on
h3=plot((xaxis_m)+skip, meanCrowdee(1:15), '-dr','MarkerSize',6,'Color',colorcrowdee,'MarkerFaceColor',colorcrowdee,'MarkerEdgeColor','k');
hold on
h4=plot((xaxis_f)+skip, meanCrowdee(16:30), '-dr','MarkerSize',6,'Color',colorcrowdee,'MarkerFaceColor',colorcrowdee,'MarkerEdgeColor','k');
hold on
errorbar((xaxis_m)+skip, meanCrowdee(1:15),meanCrowdee(1:15)-ciCrowdee(1:15,1), '-dr','MarkerSize',6,'Color',colorcrowdee,'MarkerFaceColor',colorcrowdee,'MarkerEdgeColor','k')
hold on
errorbar((xaxis_f)+skip, meanCrowdee(16:30),meanCrowdee(16:30)-ciCrowdee(16:30,1), '-dr','MarkerSize',6,'Color',colorcrowdee,'MarkerFaceColor',colorcrowdee,'MarkerEdgeColor','k')
hold off

axis([0,35,20,80])

% sorted pseudonyms:
pseudonyms_m(indexes_m);
pseudonyms_f(indexes_f);

% shorten Edinburghof....
pseudonyms_m{indexes_m(14)}='edinburgh...';

ax = gca;
ax.XTick = [xaxis_m,xaxis_f];

% set( gca(), 'XTickLabel', [pseudonyms_m(indexes_m); pseudonyms_f(indexes_f)],'fontsize',15 )

MF={'M1','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','M13','M14','M15','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15'};
set( gca(), 'XTickLabel', MF ,'fontsize',15 )

rotateXLabels( gca(), 45 )

ylabel('Mean Likability Rating (%)'),
% xlabel('Speaker pseudonym'),
xlabel('Speaker'),
legend([h1,h3],'Lab-SCA','CS-SCA')
set(gca,'FontName','times')
% set(fig, 'PaperPosition', [0 0 20 15]); % aspect ratio
set(fig, 'PaperPosition', [0 0 30 15]); % aspect ratio


% %% Save the plot
% 
savefig(fig,[path_figures,'/',namePlot,'.fig'])
saveas(fig,[path_figures,'/',namePlot],'png')
print(fig,[path_figures,'/',namePlot],'-dpdf')

