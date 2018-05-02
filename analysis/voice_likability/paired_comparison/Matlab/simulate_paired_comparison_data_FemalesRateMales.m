
% "Paired-Comparison for voice likability assessments"

% Simulate paired-comparison data
% Using my likability data of the listening test, collected using a continuous scale

clear

%% Paths
path_11='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\11\matlab';
path_16='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\16';

%% Open and prepare my scale data

load([path_11,'\data_NB_WB.mat']) % speakers are in rows and listener are in columns

% data_WB % stimuli are in rows and listener are in columns. Rows 1 to 30: NB, Rows 31 to 60: WB
nlisteners=size(data_WB,2);

% get male speakers indexes
nspeakers=size(data_WB,1)/2;
load([path_11,'\participants.mat'])
ispresent = cellfun(@(s) ~isempty(strfind('m', s)), gender);
male_i=find(ispresent==1);
nstimuli=length(male_i); % one bandwidth only, one gender only

% get listener indexes
clear ispresent
ispresent = cellfun(@(s) ~isempty(strfind('f', s)), gender);
female_i=find(ispresent==1);

% delete indexes of the two females who are not participating (SHagedorn and SMilarch)
female_i(female_i==3)=[];
female_i(female_i==12)=[];


%% Get pairs and winners

% Pairs
stimuli_indexes=1:nstimuli;
C=nchoosek(stimuli_indexes,2); % NaNs included (listener=speaker)


%% Create matrix of choice frequencies

choice_frequencies=zeros(nstimuli);
countties=0;

for j=1:length(female_i) % for each listener
    
    % local preference matrix for listener j
    choice_frequencies_listener=zeros(nstimuli); 
    
    for p=1:length(C) % go through all pairs
        
        pair=C(p,:);
        
        pair_spk=male_i(pair);
        
        scores_pair=data_WB(pair_spk,female_i(j));
        
        if ~isnan(sum(scores_pair)) % if no nan is found
            
            if ~(scores_pair(1)==scores_pair(2)) % if no tie is found (repeated scores)
                
                [sorted_scores_pair,sort_indexes]=sort(scores_pair,'descend');
                
                sorted_pair=pair(sort_indexes);
                
                % Row stimuli are chosen over column stimuli
                choice_frequencies(sorted_pair(1),sorted_pair(2))=choice_frequencies(sorted_pair(1),sorted_pair(2))+1;
                choice_frequencies_listener(sorted_pair(1),sorted_pair(2))=choice_frequencies_listener(sorted_pair(1),sorted_pair(2))+1;
                
            else
                disp(['Detected repeated scores, listener:',num2str(female_i(j)),' pair: ',num2str(C(p,1)),'-',num2str(C(p,2)),', scores: ',num2str(scores_pair(1)),' & ',num2str(scores_pair(2))])
                countties=countties+1;
                
                % randomly sort: [1 2] or [2 1]
                sorted_pair=pair(randperm(2));
                
                % Row stimuli are chosen over column stimuli
                choice_frequencies(sorted_pair(1),sorted_pair(2))=choice_frequencies(sorted_pair(1),sorted_pair(2))+1;
                choice_frequencies_listener(sorted_pair(1),sorted_pair(2))=choice_frequencies_listener(sorted_pair(1),sorted_pair(2))+1;
            end
        end
        
    end % end go through all pairs
    
    % save matrix for this listener j
    preferencematrix=choice_frequencies_listener;
    save([path_16,'/matlab/preferenceMatricesSimulated/preferencematrix_',name{female_i(j)},'.mat'],'preferencematrix')
    clear preferencematrix choice_frequencies_listener
    
end % end for each listener


%% Save paired-comparison data (matrix of choice frequencies) for matlab and for R

% Matrix of choice frequencies. Pairs of 15 stimuli: only WB, only male speakers, only female listeners, only 13 female listeners. Row stimuli are chosen over column stimuli

fileID = fopen([path_16,'/R/preferencematrix_from_directscaling_FemalesRateMales.csv'],'w');
colnames='';
strcode='';
for i=1:14
    colnames=[colnames,name{male_i(i)},','];
    strcode=[strcode,'%u, '];
end
colnames=[colnames,name{male_i(15)}];
strcode=[strcode,'%u\n'];
fprintf(fileID,'%s\n',colnames);
fprintf(fileID,strcode, choice_frequencies');   % nstimuli x nstimuli
fclose(fileID);

