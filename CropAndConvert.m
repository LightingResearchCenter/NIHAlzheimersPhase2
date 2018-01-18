%% Reset MATLAB
close all
clear
clc
 
%% Map file paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');

project = '\\ROOT\projects\NIH Alzheimers\NIH Alzheimers Phase two study\Actiware Data\Actiware files for IV and IS';
original = fullfile(project,'original');

orgListing = dir(fullfile(original,'*actigraph.xlsx'));

fileNames = {orgListing.name}';
orgFilePaths = fullfile(original,fileNames);
subjects  = str2double(regexprep(fileNames,'\D*(\d*)\D*','$1'));

dbName  = [timestamp,'.mat'];
dbPath  = fullfile(project,dbName);

ls = dir([project,filesep,'*.mat']);
if ~isempty(ls)
[~,idxMostRecent] = max(vertcat(ls.datenum));
prevDataName = ls(idxMostRecent).name;
prevDataPath = fullfile(project,prevDataName);
load(prevDataPath,'dataArray');
prevSubjects = [dataArray.subject]';
prevSessions = {dataArray.session}';
else
    dataArray = struct;
    prevSubjects = [];
    prevSessions = {};
end

%% Crop files
ii = numel(dataArray) + 1;
for iFile = 1:numel(orgFilePaths)
    
    thisPath	= orgFilePaths{iFile};
    [~,sheets]	= xlsfinfo(thisPath);
    thisSubject = subjects(iFile);
    idxSubject = prevSubjects == thisSubject;
    
    for iSheet = 1:numel(sheets)
        thisSheet = sheets{iSheet};
        thisSession = matlab.lang.makeValidName(thisSheet);
        idxSession = strcmp(thisSession,prevSessions);
        
        if ~any(idxSubject & idxSession)
        data = importActiwatchExcel(thisPath,thisSheet);
        titleText = [num2str(thisSubject),' - ',thisSheet];
        data = crop(data,titleText);
        
        dataArray(ii).subject = thisSubject;
        dataArray(ii).session = thisSession;
        dataArray(ii).data = data;
        
        ii = ii + 1;
        % Save converted data to file
        save(dbPath,'dataArray');
        end
    end
end

save(dbPath,'dataArray');
