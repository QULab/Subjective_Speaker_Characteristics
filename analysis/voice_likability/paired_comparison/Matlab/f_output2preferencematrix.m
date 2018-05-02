
% script to create the dissimilarity matrix from the java output (.txt)
% one preference matrix per listener, and then combine all matrices
% externally
% for the BTL model in R

function f_output2preferencematrix(filename,name_males_i,path_preferenceMatrices)

structdata=importdata(filename);

pairs=structdata.textdata(:,5);
numsA=structdata.data(:,3);
numsB=structdata.data(:,4);
preferredNums=structdata.data(:,13);

nvoices=length(name_males_i);

% init dissimilarity matrix
preferencematrix=zeros(nvoices);


% From letters to int 1--16 and fill in the dissimilarity matrix
for p=1:length(pairs)
    
    spkNumA=numsA(p)+1; % same as: spkNumA=uint8(pairs{p}(1))-97+1;
    spkNumB=numsB(p)+1;% same as: spkNumB=uint8(pairs{p}(2))-97+1;
    preferredSpkNum=preferredNums(p)+1;
    
    index=find([spkNumA, spkNumB]==preferredSpkNum);
    
    % Row stimuli are chosen over column stimuli
    if index==1
        preferencematrix(spkNumA,spkNumB)=preferencematrix(spkNumA,spkNumB)+1;
    elseif index==2
        preferencematrix(spkNumB,spkNumA)=preferencematrix(spkNumB,spkNumA)+1;
    else
        error('Error: preferredSpkNum not found within spkNumA and spkNumB');
    end

end

% save dissimilarity matrix for this listener

ch=strfind(filename,'_');
listenerNum=filename(ch(3)+1:ch(4)-1);
listenerName=filename(ch(4)+1:end-4);

% Matlab
save([path_preferenceMatrices,'/','preferencematrix_',listenerNum,'_',listenerName],'preferencematrix');

% R
fileID = fopen([path_preferenceMatrices,'/','preferencematrix_',listenerNum,'_',listenerName,'.csv'],'w');
colnames='';
strcode='';
for i=1:14
    colnames=[colnames,name_males_i{i},','];
    strcode=[strcode,'%u, '];
end
colnames=[colnames,name_males_i{15}];
strcode=[strcode,'%u\n'];
fprintf(fileID,'%s\n',colnames);
fprintf(fileID,strcode, preferencematrix'); 
fclose(fileID);









