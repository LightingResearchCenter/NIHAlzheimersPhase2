function averageCS( varargin )
%averageCS Summary of this function goes here
%   Detailed explanation goes here

%% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
d12packDir      = fullfile(githubDir,'d12pack');
addpath(d12packDir);

%% Map paths
projectDir  = '\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData';
croppedDir  = fullfile(projectDir,'croppedData');
tablesDir   = fullfile(projectDir,'tables');
crossRefDir = fullfile(projectDir,'crossReference');


if nargin > 0
    dbPath = varargin{1};
else
    lsCrop = dir([croppedDir,filesep,'*.mat']);
    [~,idxMostRecent] = max(vertcat(lsCrop.datenum));
    dbPath = fullfile(croppedDir,lsCrop(idxMostRecent).name);
end

if nargin > 1
    xlsxPath = varargin{2};
else
    timestamp = datestr(now,'yyyy-mm-dd_HHMM');
    xlsxPath = fullfile(tablesDir,['average_cs_',timestamp,'.xlsx']);
end

lsCross = dir([crossRefDir,filesep,'*.xlsx']);
[~,idxMostRecent] = max(vertcat(lsCross.datenum));
subjectPath = fullfile(crossRefDir,lsCross(idxMostRecent).name);

%% Load data
load(dbPath);
S = readtable(subjectPath);
S(isnan(S.ids),:) = [];
S.startTime.TimeZone = 'America/New_York';
S.stopTime.TimeZone = 'America/New_York';

%%
nObj = numel(objArray);
meanCS = NaN(nObj,24);
h = waitbar(0,['Starting analysis of ',num2str(nObj),' objects.']);

for iObj = 1:nObj
    thisS = S(S.ids == str2double(objArray(iObj).ID) & S.sn == objArray(iObj).SerialNumber,:);
    TF = false(size(thisS.ids));
    for iS = 1:height(thisS)
        TF(iS) = timeBetween(objArray(iObj).Time, thisS.startTime(iS), thisS.stopTime(iS));
    end
    thisS = thisS(TF, :);
    
    if ~isempty(thisS)
        objArray(iObj).Session(1).Name     = thisS.session(1);
        objArray(iObj).Session(1).Group    = thisS.group(1);
        objArray(iObj).Session(1).Protocol = thisS.protocol(1);
    else
        objArray(iObj).Session(1).Name     = 'unknown';
        objArray(iObj).Session(1).Group    = 'unknown';
        objArray(iObj).Session(1).Protocol = 'unknown';
    end
    
    lux10idx = objArray(iObj).Illuminance >= 10;
    if(iObj == 283)
        continue;
    end
    meanCS(iObj,:) = (objArray(iObj).hourly(objArray(iObj).CircadianStimulus, 'mean', lux10idx))';
    
    waitbar(iObj/nObj,h,[num2str(iObj),' of ',num2str(nObj),' objects analyzed.']);
end

waitbar(1,h,'Analysis complete saving to disk...');

%%
id = {objArray.ID}';
metaData = vertcat(objArray.Session);
session  = vertcat(metaData(:).Name);
group    = vertcat(metaData(:).Group);
protocol = vertcat(metaData(:).Protocol);
T = table(id, group, session, protocol, meanCS);

T = sortrows(T,{'group','id','protocol','session'});

%%
writetable(T,xlsxPath);

delete(h);

end


function TF = timeBetween(Time, startTime, stopTime)
TF = any(Time >= startTime & Time <= stopTime);
end


