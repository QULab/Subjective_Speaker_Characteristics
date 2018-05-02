
% Script to combine preference matrices of all listeners
% They are created with f_output2preferencematrix and stored in path_preferenceMatrices

function f_combinePreferenceMatrices(path_preferenceMatrices,name_males_i)

cd(path_preferenceMatrices)

files=dir;

choice_frequencies=zeros(length(name_males_i));

% Go through all saved matrices and sum up in choice_frequencies
for i=3:length(files)
    
    if ~isempty(strfind(files(i).name,'preferencematrix_')) && ~isempty(strfind(files(i).name,'.mat')) % only load if .mat file
        load(files(i).name)
        
        %     sum(sum(preferencematrix)) % should be 105
        
        choice_frequencies=choice_frequencies+preferencematrix;
        
    end
end

% save the total choice_frequencies matrix

% Matlab
save([path_preferenceMatrices,'/','choiceFrequencies'],'choice_frequencies');

% R
fileID = fopen([path_preferenceMatrices,'/','choiceFrequencies.csv'],'w');
colnames='';
strcode='';
for i=1:14
    colnames=[colnames,name_males_i{i},','];
    strcode=[strcode,'%u, '];
end
colnames=[colnames,name_males_i{15}];
strcode=[strcode,'%u\n'];
fprintf(fileID,'%s\n',colnames);
fprintf(fileID,strcode, choice_frequencies'); 
fclose(fileID);



