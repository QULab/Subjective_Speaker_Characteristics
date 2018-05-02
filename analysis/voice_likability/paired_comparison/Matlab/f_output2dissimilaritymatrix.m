
% script to create the dissimilarity matrix from the java output (.txt)
% one dissimilarity matrix per listener
% for the INDSCAL model in R
% filename = files(i).name

function f_output2dissimilaritymatrix(filename,name_males_i,path_dissimilatityMatrices)

structdata=importdata(filename);

nvoices=length(name_males_i);
pairs=structdata.textdata(:,5);
absDistances=structdata.data(:,14);
numsA=structdata.data(:,3);
numsB=structdata.data(:,4);

% init dissimilarity matrix
dissimilaritymatrix=zeros(nvoices);


% From letters to int 1--16 and fill in the dissimilarity matrix
for p=1:length(pairs)
    
    spkNumA=numsA(p)+1; % same as: spkNumA=uint8(pairs{p}(1))-97+1;
    spkNumB=numsB(p)+1;% same as: spkNumB=uint8(pairs{p}(2))-97+1;
    
    % % sort in order to fill the positions above the diagonal only
    % sortedSpkNum=sort([spkNumA, spkNumB]);
    % dissimilaritymatrix(sortedSpkNum(1),sortedSpkNum(2))=absDistances(p);
    
    % Fill in a symmetric dissimilarity matrix
    
    dissimilaritymatrix(spkNumA,spkNumB)=absDistances(p);
    dissimilaritymatrix(spkNumB,spkNumA)=absDistances(p);
end

 

% Save dissimilarity matrix for this listener

ch=strfind(filename,'_');
listenerNum=filename(ch(3)+1:ch(4)-1);
listenerName=filename(ch(4)+1:end-4);

% Matlab
save([path_dissimilatityMatrices,'/','dissimilaritymatrix_',listenerNum,'_',listenerName],'dissimilaritymatrix');

% R
fileID = fopen([path_dissimilatityMatrices,'/','dissimilaritymatrix_',listenerNum,'_',listenerName,'.csv'],'w');
colnames='';
strcode='';
for i=1:14
    colnames=[colnames,name_males_i{i},','];
    strcode=[strcode,'%u, '];
end
colnames=[colnames,name_males_i{15}];
strcode=[strcode,'%u\n'];
fprintf(fileID,'%s\n',colnames);
fprintf(fileID,strcode, dissimilaritymatrix'); 
fclose(fileID);















