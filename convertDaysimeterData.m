function convertDaysimeterData

%% Reset MATLAB
close all
clear
clc

%% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
d12packDir      = fullfile(githubDir,'d12pack');
addpath(d12packDir);

%% Map paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');
rootDir = '\\root\projects';
prjDir  = fullfile(rootDir,'NIH Alzheimers','NIH Alzheimers Phase two study','daysimeterData');
srcDir  = fullfile(prjDir,'originalData','unsorted');
dbName  = ['uncropped_',timestamp,'.mat'];
dbDir = fullfile(prjDir,'convertedData');
dbPath  = fullfile(dbDir,dbName);

listing = dir(srcDir);
listing = listing(~[listing.isdir],:);

filePaths = fullfile({listing.folder}',{listing.name}');

objArray = importData(filePaths);

save(dbPath,'objArray');
end

function objArray = importData(filePaths)
%IMPORTDATA Summary of this function goes here
%   Detailed explanation goes here

% Ask for time zones
tzLaunch = 'America/New_York';
tzDeploy = 'America/New_York';


% Seperate files by type
fileTag = regexprep(filePaths,'.*(DATA\.txt|LOG\.txt|\.cdf)$','$1','ignorecase');

dataIdx = strcmpi(fileTag,'DATA.txt');
logIdx  = strcmpi(fileTag,'LOG.txt');
cdfIdx  = strcmpi(fileTag,'.cdf');

dataPaths = filePaths(dataIdx);
logPaths  = filePaths(logIdx);
cdfPaths  = filePaths(cdfIdx);

nData = numel(dataPaths);
SourceData(nData,1) = d12pack.HumanData;
if exist(SourceData(nData).CalibrationPath,'file') == 2
    calPath = SourceData(nData).CalibrationPath;
else
    calPath = which('calibration_log.csv');
end

for iData = 1:nData
    thisDataPath = dataPaths{iData};
    thisLogPath = regexprep(thisDataPath,'-DATA\.txt','-LOG.txt','ignorecase');
    thisCdfPath = regexprep(thisDataPath,'-DATA\.txt','.cdf','ignorecase');
    
    if ismember(thisCdfPath,cdfPaths)
        try
            cdfData = readcdf(thisCdfPath);
            thisID  = cdfData.GlobalAttributes.subjectID;
        catch
            thisID  = 'unknown';
            display(thisCdfPath);
        end
    else
        thisID  = 'unknown';
    end
    
    if ismember(thisLogPath,logPaths)
        
        SourceData(iData).CalibrationPath = calPath;
        SourceData(iData).RatioMethod     = 'normal';
        SourceData(iData).TimeZoneLaunch  = tzLaunch;
        SourceData(iData).TimeZoneDeploy  = tzDeploy;
        
        % Import the original data
        SourceData(iData).log_info = SourceData(iData).readloginfo(thisLogPath);
        SourceData(iData).data_log = SourceData(iData).readdatalog(thisDataPath);
        
        % Add ID
        SourceData(iData).ID = thisID;
    end
end

% Remove empty data sets
idxEmpty = cellfun(@isempty,{SourceData.SerialNumber}');
SourceData(idxEmpty) = [];

objArray = SourceData;
end


function Data = readcdf(filePath)
%READCDF Summary of this function goes here
%   Detailed explanation goes here

Data = struct('Variables',[],'GlobalAttributes',[],'VariableAttributes',[]);

cdfId = cdflib.open(filePath);

fileInfo = cdflib.inquire(cdfId);

% Read in variables
nVars = fileInfo.numVars;

for iVar = 0:nVars-1
    varInfo = cdflib.inquireVar(cdfId,iVar);
    
    % Determine the number of records allocated for the first variable in the file.
    maxRecNum = cdflib.getVarMaxWrittenRecNum(cdfId,iVar);
    
    % Retrieve all data in records for variable.
    if maxRecNum > 0
        varData = cdflib.hyperGetVarData(cdfId,iVar,[0 maxRecNum+1 1]);
    else
        varData = cdflib.getVarData(cdfId,iVar,0);
    end
    
    Data.Variables.(varInfo.name) = varData;
end

% Read in attributes
nAttrs = fileInfo.numvAttrs + fileInfo.numgAttrs;

for iAttr = 0:nAttrs-1
    attrInfo = cdflib.inquireAttr(cdfId,iAttr);
    switch attrInfo.scope
        case 'GLOBAL_SCOPE'
            nEntry = cdflib.getAttrMaxgEntry(cdfId,iAttr) + 1;
            if nEntry == 1
                attrData = cdflib.getAttrgEntry(cdfId,iAttr,0);
                Data.GlobalAttributes.(attrInfo.name) = attrData;
            else
                Data.GlobalAttributes.(attrInfo.name) = cell(nEntry,1);
                for iEntry = 0:nEntry-1
                    attrData = cdflib.getAttrgEntry(cdfId,iAttr,iEntry);
                    Data.GlobalAttributes.(attrInfo.name){iEntry+1,1} = attrData;
                end
            end
        case 'VARIABLE_SCOPE'
            nEntry = cdflib.getAttrMaxEntry(cdfId,iAttr) + 1;
            for iEntry = 0:nEntry-1
                varName = cdflib.getVarName(cdfId,iEntry);
                attrData = cdflib.getAttrEntry(cdfId,iAttr,iEntry);
                Data.VariableAttributes.(varName).(attrInfo.name) = attrData;
            end
        otherwise
            error('Unknown attribute scope.');
    end
end

% Clean up
cdflib.close(cdfId)

clear cdfId
end

