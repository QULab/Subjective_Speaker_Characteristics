

clear

%% paths
path_output='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\16\Listeners and Listening test\Eclipse_output';
path_matlab='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\16\Matlab';
path_dissimilatityMatrices=[path_matlab,'\dissimilatityMatrices'];
path_preferenceMatrices=[path_matlab,'\preferenceMatrices'];
path_11='D:\Users\fernandez.laura\Documents\Work\WP1_Data_collection\11\matlab';

addpath(path_matlab)


%% get male speakers indexes 
nspeakers = 15;
load([path_11,'\participants.mat'])
ispresent = cellfun(@(s) ~isempty(strfind('m', s)), gender);
male_i=find(ispresent==1);
name_males_i=name(male_i);

%% Call the corresponding scripts from the filenames in the java output folder

cd(path_output)
files=dir;
for i=3:length(files)
    if (~isempty(strfind(files(i).name,'results_')) && ~isempty(strfind(files(i).name,'.txt')))
        
        % Dissimilarity matrix
       f_output2dissimilaritymatrix(files(i).name,name_males_i,path_dissimilatityMatrices);
        
       % Preference matrix
       f_output2preferencematrix(files(i).name,name_males_i,path_preferenceMatrices);
       
    end
end


%% Combine preference matrices of all listeners (sum)
f_combinePreferenceMatrices(path_preferenceMatrices,name_males_i)

%% Combine dissimilarity matrices of all listeners (average)
f_combineDissimilarityMatrices(path_dissimilatityMatrices,name_males_i)



