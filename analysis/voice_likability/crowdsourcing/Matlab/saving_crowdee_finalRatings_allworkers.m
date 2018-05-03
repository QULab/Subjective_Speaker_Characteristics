
% Check answers from real job (sympathy, scaling)
% Get answers which have to be rejected, based on the trap questions in the real job
% Do not reject based on the qualification job

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

%% Crowdee. Read answers from qualification job
filename = 'Answers_Job_189_2017-02-06T09-26-48.863Z.csv';
[~,~,Worker_q,Status_q,~,~,~,~,~,~,~,~,Answer1fileId_q,Answer2selections_q,Answer3selections_q,Answer4selections_q,Answer5freetext_q]= f_read_answers189([path_results,'/',filename]);

%% Crowdee. Read answers from real job
cd(path_results)
files=dir;

% Sort files by date and take the most recent file
setfiles = [files(:).datenum].';
[setfiles,setfiles] = sort(setfiles,'descend');
setfiles = {files(setfiles).name};

filenumber=7;
filename=setfiles{filenumber} % 191: males
[AnswerId_m,~,Worker_m,Status_m,~,~,~,~,~,~,~,~,Answer1sliders_m,Answer2sliders_m,Answer3sliders_m,Answer4sliders_m,Answer5sliders_m,Answer6sliders_m,Answer7sliders_m,Answer8sliders_m,DynamicContentaudio1_m,DynamicContentaudio2_m,DynamicContentaudio3_m,DynamicContentaudio4_m,DynamicContentaudio5_m,DynamicContentaudio6_m,DynamicContentaudio7_m,DynamicContentaudio8_m] = f_read_answers190_191([path_results,'/',filename]);

filenumber=6;
filename=setfiles{filenumber} % 190: females
[AnswerId_f,~,Worker_f,Status_f,~,~,~,~,~,~,~,~,Answer1sliders_f,Answer2sliders_f,Answer3sliders_f,Answer4sliders_f,Answer5sliders_f,Answer6sliders_f,Answer7sliders_f,Answer8sliders_f,DynamicContentaudio1_f,DynamicContentaudio2_f,DynamicContentaudio3_f,DynamicContentaudio4_f,DynamicContentaudio5_f,DynamicContentaudio6_f,DynamicContentaudio7_f,DynamicContentaudio8_f] = f_read_answers190_191([path_results,'/',filename]);











% From the accepted trustworthy workers:
% 1. age and gender?
% 2. were they noisy?

Worker = [Worker_q; Worker_m; Worker_f];
length(unique(Worker)) % 96
length(unique([Worker_m; Worker_f])) % 69
length([Worker_m; Worker_f]) % 127


% what workers been rejected?
counter=1;
for i=1:length(Worker_m)
    if strcmp(Status_m{i},'REJECTED')
        disp([Worker_m{i}, ' was rejected in the male job ****** '])
    else
        W{counter,1}=Worker_m{i};
        counter = counter+1;
    end
end
counter=1;
for i=1:length(Worker_f)
    if strcmp(Status_f{i},'REJECTED')
        disp([Worker_f{i}, ' was rejected in the female job ****** '])
    else
        W{counter,2}=Worker_f{i};
        counter = counter+1;
    end
end

uW = unique(W); % unique workers that are trustworthy
length(uW) % = 68

% counter how many workers do the other-gender job?
both=0;
onlyone=0;
for i=1:length(uW)
    where_m = cellfun(@(s) ~isempty(strfind(uW{i},s)), W(:,2));
    where_f = cellfun(@(s) ~isempty(strfind(uW{i},s)), W(:,1));
    
    if sum([where_m ; where_f])==2
        both=both+1; % counter - performed both jobs
    elseif sum([where_m ; where_f])==1
        onlyone=onlyone+1; % counter - performed only one job
    else
        disp('algo va mal...')
    end
end



% counter how many workers do the other-gender job?
c=0;
for i=1:length(Worker_m)
    where = cellfun(@(s) ~isempty(strfind(Worker_m{i},s)), Worker_f);
    if sum(where)==1 % has been found
        c=c+1;
        
        if strcmp(Status_m{i},'REJECTED')
            disp([Worker_m{i}, ' performed 2 jobs and was rejected in the male job'])
        end
        
        if strcmp(Status_f{where},'REJECTED')
            disp([Worker_m{i}, ' performed 2 jobs and was rejected in the female job'])
        end
        
    elseif sum(where)==0 % not found
        disp([Worker_m{i}, ' performed the male job only'])
        if strcmp(Status_m{i},'REJECTED')
            disp(['***', Worker_m{i}, ' performed the male job only and was rejected'])
        end
    elseif sum(where)==2 % has been found
        disp([Worker_m{i}, ' has been found 2 times! \\\\\\\\\\\\'])
    end
end
c=0;
for i=1:length(Worker_f)
    
    where = cellfun(@(s) ~isempty(strfind(Worker_f{i},s)), Worker_m);
    
    if sum(where)==1 % has been found
        c=c+1;
        
    elseif sum(where)==0 % not found
        disp([Worker_f{i}, ' performed the female job only'])
        if strcmp(Status_f{i},'REJECTED')
            disp(['***', Worker_f{i}, ' performed the female job only and was rejected'])
        end
    elseif sum(where)==2 % has been found
        disp([Worker_f{i}, ' has been found 2 times! \\\\\\\\\\\\'])
    end
end

% 69 workers in total (trustworthy and not trustworthy)
% 52 workers did both jobs and were accepted in both jobs -> 52 * 2 = 104 submissions accepted
% 5 workers did the male job only and were all accepted -> 5 submissions accepted
% 6 workers did the male job only and were all accepted -> 6 submissions accepted
% 2 workers did both jobs but were rejected in the males job -> 2 submissions accepted + 2 submissions rejected
% 3 workers did both jobs but were rejected in the females job -> 3 submissions accepted + 3 submissions rejected
% 1 worker did both jobs but was rejected in both -> 0 submissions accepted + 2 submissions rejected

% Total submissions (accepted only): 104+5+6+2+3+0 = 120
% Total submissions = 120 + 2+3+2 = 127

% Total trustworthy workers: 52 + 5 + 6 + 2 + 3 = 68.



% From the accepted trustworthy workers:
% 1. age and gender?
% 2. were they noisy?

% look for uW in qualif and get age and gender
% c=0
for i=1:length(uW)
    where = cellfun(@(s) ~isempty(strfind(uW{i},s)), Worker_q);
%     if sum(where)==0
%         disp(['!!!!!!!!!! ', uW{i},' did not perform the qualif job'])
%     elseif sum(where)==1
%         c=c+1;
%     end
    
% All uW performed the qualif job only once

gender(i)=Answer4selections_q(i); % gender
age(i)=Answer5freetext_q(i); % age
end
sum(gender==1) % = 30*100/length(uW) - Female
sum(gender==0) % = 38*100/length(uW) - Male
mean(age)
std(age)




%% Get stimulus

% join all DynamicContentaudio
DynamicContentaudio_m=[DynamicContentaudio1_m,DynamicContentaudio2_m,DynamicContentaudio3_m,DynamicContentaudio4_m,DynamicContentaudio5_m,DynamicContentaudio6_m,DynamicContentaudio7_m,DynamicContentaudio8_m];
DynamicContentaudio_f=[DynamicContentaudio1_f,DynamicContentaudio2_f,DynamicContentaudio3_f,DynamicContentaudio4_f,DynamicContentaudio5_f,DynamicContentaudio6_f,DynamicContentaudio7_f,DynamicContentaudio8_f];

where_m = cellfun(@(s) ~isempty(strfind('REJECTED',s)), Status_m); % sum(where_m)= 3 for 191, males
Worker_m(where_m)=[];
DynamicContentaudio_m(where_m,:)=[];
size(DynamicContentaudio_m)

where_f = cellfun(@(s) ~isempty(strfind('REJECTED',s)), Status_f);  % sum(where_f)= 4 for 190, females
Worker_f(where_f)=[];
DynamicContentaudio_f(where_f,:)=[];
size(DynamicContentaudio_f)



% load mapping
load([path_json,'\mapping.mat']);

cellfind = @(string)(@(cell_contents)(strcmp(string,cell_contents)));

% get filename of stimuli played
for i=1:size(DynamicContentaudio_m,1) % stimuli in the same question number for different workers
    for j=1:size(DynamicContentaudio_m,2) % % stimuli in the same job
        pos = cellfun(cellfind(DynamicContentaudio_m(i,j)),mapping);
        stimulus_m{i,j}=mapping{find(pos==1)-90};
    end
end

size(stimulus_m)

for i=1:size(DynamicContentaudio_f,1) % stimuli in the same question number for different workers
    for j=1:size(DynamicContentaudio_f,2) % % stimuli in the same job
        pos = cellfun(cellfind(DynamicContentaudio_f(i,j)),mapping);
        stimulus_f{i,j}=mapping{find(pos==1)-90};
    end
end

size(stimulus_f)

% stimulus-> rows = different worker. column -> different question in the same job

%% Get answers
% join all Answersliders
answers_m=[Answer1sliders_m,Answer2sliders_m,Answer3sliders_m,Answer4sliders_m,Answer5sliders_m,Answer6sliders_m,Answer7sliders_m,Answer8sliders_m];
answers_m(where_m,:)=[];
answers_f=[Answer1sliders_f,Answer2sliders_f,Answer3sliders_f,Answer4sliders_f,Answer5sliders_f,Answer6sliders_f,Answer7sliders_f,Answer8sliders_f];
answers_f(where_f,:)=[];

answers=[answers_m; answers_f];

% size(answers_m)   % size(answers_f)   % size(answers)





%% Mean of answers for each speaker

% unique stimulus (wav files)
stimulus = [stimulus_m; stimulus_f]; % size(stimulus)
[ustimulus,bb,cc]=unique(stimulus);

% remove traps
iTrap = strfind(ustimulus, 'trap');
posTrap = find(not(cellfun('isempty', iTrap)));
ustimulus(posTrap)=[];

size(ustimulus)



% % Loop: find ratings to stimulus
% for i=1:length(ustimulus)
%     istimulus = strfind(stimulus, ustimulus{i});
%     posStimulus = find(not(cellfun('isempty', istimulus)));
%     
%     % checking: stimulus(posStimulus) - ok
%     
%     % Not all stimuli have the same number of answers. so we just compute the mean. we can't do yet: ratings(i,:)=answers(posStimulus);
%     
%     x = answers(posStimulus); % ratings for stimulus(posStimulus), same as ustimulus{i}
%     
%     meanRating(i,1)=mean(x);
%     stdevRating(i,1)=std(x);
%     maxRating(i,1)=max(x);
%     minRating(i,1)=min(x);
%     
%     SEM = std(x)/sqrt(length(x));               % Standard Error
%     ts = tinv([0.025  0.975],length(x)-1);      % T-Score
%     CI = mean(x) + ts*SEM;
%     cimeanRating(i,:)=CI;
%     
% end
% 
% save([path_matlab,'\crowdee_results_',filename(1:end-4),'.mat'],'ustimulus','meanRating','stdevRating','maxRating','minRating','cimeanRating');  %%



%% build data "manually" for the computation of ICC in R

% *** For ICC, get big ratings matrix with speakers x workers [30*68]
uW=unique([Worker_m, Worker_f]);
cs_ratings = zeros(length(ustimulus),length(uW))*NaN;  % Worker_m, Worker_f rejected were filtered out above

for i=1:length(uW) % for each accepted worker
    
    % look for what stimuli he rated
    where = cellfun(@(s) ~isempty(strfind(uW{i},s)), [Worker_m; Worker_f]);
    
    stimuliw = stimulus(where,:);
    stimuli = reshape(stimuliw, size(stimuliw,1)*size(stimuliw,2),1);
    
    answ_w = answers(where,:);
    answ = reshape(answ_w, size(answ_w,1)*size(answ_w,2),1);
    
    
    for j=1:length(stimuli)
        istimulus = strfind(ustimulus, stimuli{j});
        posStimulus = find(not(cellfun('isempty', istimulus)));
        
        if ~isempty(posStimulus) % stimuli{j} was not trap
            % disp(stimuli{j})
            cs_ratings(posStimulus,i) = answ(j); % register answer
        end
    end
end

csvwrite([path_matlab,'/cs_ratings.csv'],cs_ratings);
