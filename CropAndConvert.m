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

%% Crop files
dataArray = struct;
ii = 1;
for iFile = 1:numel(orgFilePaths)
    
    thisPath	= orgFilePaths{iFile};
    thisSubject = subjects(iFile);
    [~,sheets]	= xlsfinfo(thisPath);
    
    for iSheet = 1:numel(sheets)
        thisSheet = sheets{iSheet}; 
        
        data = importActiwatchExcel(thisPath,thisSheet);
        titleText = [num2str(thisSubject),' - ',thisSheet];
        data = crop(data,titleText);
        
        dataArray(ii).subject = thisSubject;
        dataArray(ii).session = matlab.lang.makeValidName(thisSheet);
        dataArray(ii).data = data;
        
        ii = ii + 1;
        % Save converted data to file
        save(dbPath,'dataArray');
    end
    
end

