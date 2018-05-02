
% Script to combine dissimilarity matrices of all listeners (making the
% average of the distances)
% They are created with f_output2dissimilaritymatrix and stored in path_dissimilatityMatrices
% This function is called from main_GenerateMatricesFromJavaOutput

function f_combineDissimilarityMatrices(path_dissimilatityMatrices,name_males_i)

cd(path_dissimilatityMatrices)

files=dir;

dissimilarity_total=zeros(length(name_males_i));

 
% Go through all saved matrices and sum them, element by element
 count=0;
for i=3:length(files)
    
    if ~isempty(strfind(files(i).name,'dissimilaritymatrix_')) && ~isempty(strfind(files(i).name,'.mat')) % only load if .mat file
        
        load(files(i).name);
        
         dissimilarity_total=dissimilarity_total+dissimilaritymatrix;
        
         count=count+1;
    end
end

% Compute the average of all dissimilarity matrices
 dissimilarity_total=dissimilarity_total/count;
 

% save the total choice_frequencies matrix

% Matlab
save([path_dissimilatityMatrices,'/Dissimilarity_Total/','dissimilarity_total'],'dissimilarity_total');

% R
fileID = fopen([path_dissimilatityMatrices,'/Dissimilarity_Total/','dissimilarity_total.csv'],'w');
colnames='';
strcode='';
for i=1:14
    colnames=[colnames,name_males_i{i},','];
    strcode=[strcode,'%u, '];
end
colnames=[colnames,name_males_i{15}];
strcode=[strcode,'%u\n'];
fprintf(fileID,'%s\n',colnames);
fprintf(fileID,strcode, dissimilarity_total'); 
fclose(fileID);



