function convertActiwatchData
%CONVERTACTIWATCHDATA Summary of this function goes here
%   Detailed explanation goes here

% Map file paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');

subjectPath = '\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\subject_inventory_2017-10-08.xlsx';

project = '\\ROOT\projects\NIH Alzheimers\NIH Alzheimers Phase two study\Actiware Data\Actiware files for IV and IS';
original = fullfile(project,'original');

orgListing = dir(fullfile(original,'*actigraph.xlsx'));

fileNames = {orgListing.name}';
orgFilePaths = fullfile(original,fileNames);
subjects  = str2double(regexprep(fileNames,'\D*(\d*)\D*','$1'));

dbName  = [timestamp,'.mat'];
dbPath  = fullfile(project,dbName);

% Import subject table
S = readtable(subjectPath);
S(isnan(S.ids),:) = [];
S.startTime.TimeZone = 'America/New_York';
S.stopTime.TimeZone = 'America/New_York';

% Convert data
dataArray = struct;
ii = 1;
nFile = numel(orgFilePaths);
h = waitbar(0, ['Please wait processing file 0 of ',num2str(nFile)]);
for iFile = 1:nFile
    waitbar(iFile/nFile, h, ['Please wait processing file ',num2str(iFile),' of ',num2str(nFile)]);
    thisPath	= orgFilePaths{iFile};
    [~,sheets]	= xlsfinfo(thisPath);
    thisSubject = subjects(iFile);
    
    for iSheet = 1:numel(sheets)
        thisSheet = sheets{iSheet};
        thisSession = matlab.lang.makeValidName(thisSheet);
        
        switch thisSession
            case 'BaseActive'
                thisSession = 'baseline';
                thisProtocol = 'active';
            case 'BasePlacebo'
                thisSession = 'baseline';
                thisProtocol = 'placebo';
            case 'Baseline'
                thisSession = 'baseline';
                thisProtocol = '';
            case 'IntActive'
                thisSession = 'intervention';
                thisProtocol = 'active';
            case 'IntPlacebo'
                thisSession = 'intervention';
                thisProtocol = 'placebo';
            case 'IntWeek3'
                thisSession = 'week3';
                thisProtocol = '';
            case 'IntWeek9'
                thisSession = 'week9';
                thisProtocol = '';
            case 'IntWeek17'
                thisSession = 'week17';
                thisProtocol = '';
            case 'IntWeek24'
                thisSession = 'week24';
                thisProtocol = '';
            otherwise
                error(['Unknown session: ',thisSession]);
        end
        
        data = importActiwatchExcel(thisPath,thisSheet);
        if ~isempty(data)
            thisS = S(S.ids == thisSubject & strcmpi(S.session,thisSession) & strcmpi(S.protocol,thisProtocol),:);
            if isempty(thisS) || isempty(thisS.startTime) || isempty(thisS.stopTime)
                warning(['Matching record not found for subject: ',num2str(thisSubject),' ',thisSession,' ',thisProtocol]);
            else
                thisGroup = thisS.group{1};
                data.Observation = data.DateTime >= thisS.startTime(1) & data.DateTime <= thisS.stopTime(1);
                data.Compliance = ~(isnan(data.Activity) | strcmp(data.IntervalStatus,'EXCLUDED')) & data.Observation;
                
                dataArray(ii).subject = thisSubject;
                dataArray(ii).group = thisGroup;
                dataArray(ii).session = thisSession;
                dataArray(ii).protocol = thisProtocol;
                dataArray(ii).data = data;
                
                ii = ii + 1;
            end
        end
    end
end

delete(h);

save(dbPath,'dataArray');

end

