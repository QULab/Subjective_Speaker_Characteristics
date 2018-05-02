

clear

%% Paths
path_results='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\17\csv-Crowdee';
path_matlab='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\17\Matlab';
addpath(path_matlab)


%% Need to download the new csv
filename = 'Answers_Job-157_2016-10-25T08-50-16Z_1-1682.csv';
[AnswerId,~,Worker,Status,~,~,TaskStartedTime,~,~,~,~,~,Answer2selections,Answer3sliders,Answer4selections,DynamicContentaudioSamples] = f_read_answers157_final([path_results,'/final/',filename]);

%% From accepted answers, return sliderValue and filename

Status=cellfun(@(s) strrep( s,'"',''), Status,'un',0);


cellfind = @(string)(@(cell_contents)(strcmp(string,cell_contents)));
a = cellfun(cellfind('ACCEPTED'),Status);
aa = cellfun(cellfind('AUTOMATICALLY_ACCEPTED'),Status);
rr = cellfun(cellfind('REJECTED'),Status); 
ss = cellfun(cellfind('SUBMITTED'),Status);
disp(['Number of accepted answers = ',num2str(sum(a))]);
disp(['Number of automatically accepted answers = ',num2str(sum(aa))]);
disp(['Number of rejected answers = ',num2str(sum(rr))]);
disp(['Number of submitted answers = ',num2str(sum(ss))]);

accepted_index=a+aa; % accepted and automatically accepetd

% sum(accepted_index) % <- should be = 105*13 = 1365

%  "leftContent":"A ist sympathischer als B",
%  "rightContent":"B ist sympathischer als A"
% min=0
% max = 200
% slider_val=Answer3sliders(find(accepted_index==1)); % only accepted
slider_val=Answer3sliders; % ALL answers



% Convert url audio sample to pair number
% dcontent = DynamicContentaudioSamples(find(accepted_index==1)); % only accepted
dcontent = DynamicContentaudioSamples; % ALL answers
dcontent = cellfun(@(s) strrep( s,'"',''), dcontent,'un',0);

load('parsed_urls_Production-mat.mat'); % parsed


for i=1:length(dcontent)
    ispresent = cellfun(cellfind([dcontent{i}]),parsed(:,2));
    ispresent = cellfun(@(s) ~isempty(strfind([dcontent{i}], s)), parsed(:,2));
    pairwavname{i,1}=parsed(ispresent,1);
end


%% Save variables with timestamp
c = clock;
save([path_matlab,'\pair_val__',sprintf('Day%02d%02d_Hour%02d%02d', c(3), c(2), c(4),c(5))],'pairwavname','slider_val');


