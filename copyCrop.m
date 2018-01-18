function [ output_args ] = copyCrop( input_args )
%COPYCROP Summary of this function goes here
%   Detailed explanation goes here

% Enable dependencies ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
[githubDir,~,~] = fileparts(pwd);
d12packDir = fullfile(githubDir,'d12pack');
addpath(d12packDir);
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

% Map paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');
rootDir      = '\\root\projects';
projectDir   = fullfile(rootDir,'NIH Alzheimers','NIH Alzheimers Phase two study','daysimeterData');
convertedDir = fullfile(projectDir,'convertedData');
croppedDir   = fullfile(projectDir,'croppedData');
archiveDir   = fullfile(croppedDir, 'archive');

% Check if folders exist, if not create them
if exist(convertedDir, 'dir') == 0
    mkdir(convertedDir);
end
if exist(croppedDir, 'dir') == 0
    mkdir(croppedDir);
end
if exist(archiveDir, 'dir') == 0
    mkdir(archiveDir);
end

% Get list of files in folders
convertedLs = dir(fullfile(convertedDir, '*.mat'));
croppedLs   = dir(fullfile(croppedDir, '*.mat'));

% Check if converted files exist
if isempty(convertedLs)
    error('No files exist in ''convertedData'', convert files first.')
end

% Check if croppedData folder is not empty
if ~isempty(croppedLs)
    currentPaths = fullfile(croppedDir,{croppedLs.name}');
    % Move cropped files to archive folder
    for iFile = 1:numel(currentPaths)
        movefile(currentPaths{iFile},archiveDir);
    end
end

% Find newested converted file
[~,idxMax] = max([convertedLs.datenum]);
convertedDbPath  = fullfile(convertedDir, convertedLs(idxMax).name);
% Copy converted file to croppedData folder
currentDbPath = regexprep(convertedDbPath,'(.*)convertedData\\uncropped(.*)','$1croppedData\\cropped$2');
copyfile(convertedDbPath,currentDbPath);

% Get list of files in archive folder
archiveLs = dir(fullfile(archiveDir, '*.mat'));

% Find newested archived file
[~,idxMax] = max([archiveLs.datenum]);
archivedDbPath  = fullfile(archiveDir, archiveLs(idxMax).name);

% Load objArray from archived file
temp = load(archivedDbPath);
archivedObjArray = temp.objArray;

% Load objArray from current file
temp = load(currentDbPath);
objArray = temp.objArray;

% Create summary of meta-data from archived file
archiveMeta = table;
archiveMeta.ID = {archivedObjArray.ID}';
archiveMeta.StartTime = NaT(size(archiveMeta.ID),'TimeZone','local');
for iArchObj = 1:numel(archivedObjArray)
    archiveMeta.StartTime(iArchObj) = archivedObjArray(iArchObj).Time(1);
end
archiveMeta.SerialNumber = [archivedObjArray.SerialNumber]';

% Iterate through current file and compare to archive file
for iObj = 1:numel(objArray)
    thisID           = objArray(iObj).ID;
    thisSerialNumber = objArray(iObj).SerialNumber;
    thisStartTime    = objArray(iObj).Time(1);
    
    % Find matching object in archive
    idxMatch = strcmp(thisID,archiveMeta.ID) & (thisSerialNumber == archiveMeta.SerialNumber) & (thisStartTime == archiveMeta.StartTime);
    
    % If any archive match copy drop to current object
    if any(idxMatch)
        objArray(iObj).Observation = archivedObjArray(idxMatch).Observation;
        objArray(iObj).Compliance  = archivedObjArray(idxMatch).Compliance;
        objArray(iObj).Error       = archivedObjArray(idxMatch).Error;
        objArray(iObj).BedLog      = archivedObjArray(idxMatch).BedLog;
        objArray(iObj).WorkLog     = archivedObjArray(idxMatch).WorkLog;
        objArray(iObj).Session     = archivedObjArray(idxMatch).Session;
    end
end

% Create summary of meta-data from current file
currentMeta = table;
currentMeta.ID = {objArray.ID}';
currentMeta.ID = str2double(currentMeta.ID);
currentMeta.StartTime = NaT(numel(objArray),1,'TimeZone','local');
for iObj = 1:numel(objArray)
    currentMeta.StartTime(iObj) = objArray(iObj).Time(1);
end
currentMeta.SerialNumber = [objArray.SerialNumber]';

% Sort data by subject ID then by start time
[~,idxSort] = sortrows(currentMeta);
objArray = objArray(idxSort);

% Save current data to file
save(currentDbPath, 'objArray');

end

