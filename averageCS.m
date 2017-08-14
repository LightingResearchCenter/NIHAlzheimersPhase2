function averageCS( varargin )
%averageCS Summary of this function goes here
%   Detailed explanation goes here

%% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
d12packDir      = fullfile(githubDir,'d12pack');
addpath(d12packDir);

%%
if nargin > 0
    dbPath = varargin{1};
else
    dbPath = '\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\convertedData\complete-crop_2017-07-19_1417.mat';
end

if nargin > 1
    xlsxPath = varargin{2};
else
    timestamp = datestr(now,'yyyy-mm-dd_HHMM');
    xlsxPath = ['\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\tables\average_cs_',timestamp,'.xlsx'];
end

subjectPath = '\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\subject_inventory_2017-07-17_0827.xlsx';

%% Load data
load(dbPath);
S = readtable(subjectPath);
S(isnan(S.ids),:) = [];
S.startTime.TimeZone = 'America/New_York';
S.stopTime.TimeZone = 'America/New_York';

%%
meanCS = NaN(numel(objArray),24);
for iObj = 1:numel(objArray)
    thisS = S(S.ids == str2double(objArray(iObj).ID) & S.sn == objArray(iObj).SerialNumber,:);
    TF = false(size(thisS.ids));
    for iS = 1:height(thisS)
        TF(iS) = timeBetween(objArray(iObj).Time, thisS.startTime(iS), thisS.stopTime(iS));
    end
    thisS = thisS(TF, :);
    objArray(iObj).Session(1).Name     = thisS.session(1);
    objArray(iObj).Session(1).Group    = thisS.group(1);
    objArray(iObj).Session(1).Protocol = thisS.protocol(1);
    
    lux10idx = objArray(iObj).Illuminance >= 10;
    meanCS(iObj,:) = (objArray(iObj).hourly(objArray(iObj).CircadianStimulus, 'mean', lux10idx))';
end

%%
id = {objArray.ID}';
metaData = vertcat(objArray.Session);
session  = vertcat(metaData(:).Name);
group    = vertcat(metaData(:).Group);
protocol = vertcat(metaData(:).Protocol);
T = table(id, group, session, protocol, meanCS);
%%
writetable(T,xlsxPath);


end


function TF = timeBetween(Time, startTime, stopTime)
TF = any(Time >= startTime & Time <= stopTime);
end


