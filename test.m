%% Reset MATLAB
close all
clear
clc

%% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
d12packDir = fullfile(githubDir,'d12pack');
addpath(d12packDir);

%% Map file paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');

projectDir = '\\ROOT\projects\NIH Alzheimers\NIH Alzheimers Phase two study\Actiware Data\Actiware files for IV and IS';

ls = dir([projectDir,filesep,'*.mat']);
[~,idxMostRecent] = max(vertcat(ls.datenum));
dataName = ls(idxMostRecent).name;
dataPath = fullfile(projectDir,dataName);

xlsxPath1 = fullfile(projectDir,'tables',[timestamp,'_ISIV-NaN-Replacement.xlsx']);
xlsxPath2 = fullfile(projectDir,'tables',[timestamp,'_ISIV-Method2.xlsx']);

load(dataPath);

%% Initialize output
subjects = unique([dataArray.subject]);

nShortTerm = sum(subjects <  60);
nLongTerm  = sum(subjects >= 60);

shortTerm1 = table;
shortTerm1.subject = NaN(nShortTerm,1);
% 
% shortTerm1.BaseActive_Coverage_Days = NaN(nShortTerm,1);
% shortTerm1.BaseActive_IS = NaN(nShortTerm,1);
% shortTerm1.BaseActive_IV = NaN(nShortTerm,1);
% 
% shortTerm1.IntActive_Coverage_Days  = NaN(nShortTerm,1);
% shortTerm1.IntActive_IS  = NaN(nShortTerm,1);
% shortTerm1.IntActive_IV  = NaN(nShortTerm,1);
% 
% shortTerm1.BasePlacebo_Coverage_Days = NaN(nShortTerm,1);
% shortTerm1.BasePlacebo_IS = NaN(nShortTerm,1);
% shortTerm1.BasePlacebo_IV = NaN(nShortTerm,1);
% 
% shortTerm1.IntPlacebo_Coverage_Days  = NaN(nShortTerm,1);
% shortTerm1.IntPlacebo_IS  = NaN(nShortTerm,1);
% shortTerm1.IntPlacebo_IV  = NaN(nShortTerm,1);

longTerm1 = table;
longTerm1.subject = NaN(nLongTerm,1);
% 
% longTerm1.Baseline_Coverage_Days = NaN(nLongTerm,1);
% longTerm1.Baseline_IS = NaN(nLongTerm,1);
% longTerm1.Baseline_IV = NaN(nLongTerm,1);
% 
% longTerm1.IntWeek3_Coverage_Days = NaN(nLongTerm,1);
% longTerm1.IntWeek3_IS = NaN(nLongTerm,1);
% longTerm1.IntWeek3_IV = NaN(nLongTerm,1);
% 
% longTerm1.IntWeek9_Coverage_Days = NaN(nLongTerm,1);
% longTerm1.IntWeek9_IS = NaN(nLongTerm,1);
% longTerm1.IntWeek9_IV = NaN(nLongTerm,1);
% 
% longTerm1.IntWeek17_Coverage_Days = NaN(nLongTerm,1);
% longTerm1.IntWeek17_IS = NaN(nLongTerm,1);
% longTerm1.IntWeek17_IV = NaN(nLongTerm,1);
% 
% longTerm1.IntWeek24_Coverage_Days = NaN(nLongTerm,1);
% longTerm1.IntWeek24_IS = NaN(nLongTerm,1);
% longTerm1.IntWeek24_IV = NaN(nLongTerm,1);

shortTerm2 = shortTerm1;
longTerm2  = longTerm1;

iShortTerm = 1;
iLongTerm  = 1;

%% Perform analysis
for iSub = 1:numel(subjects)
    
    subject = subjects(iSub);
    idxSub = [dataArray.subject] == subject;
    thisData = dataArray(idxSub');
    
    for iSes = 1:numel(thisData)
        session = thisData(iSes).session;
        protocol = thisData(iSes).protocol;
        
        data = thisData(iSes).data;
        epoch = mode(diff(data.DateTime));
        
        complianceArray = adjustcrop(datenum(data.DateTime),data.Compliance);
        
        idxKeep1 = data.Observation & complianceArray;
        data1 = data(idxKeep1,:);
        data1.Activity(isnan(data1.Activity)) = 0;
        
        coverage1 = numel(data1.Activity)*epoch;
        [IS1,IV1] = isiv(data1.Activity,epoch);
        
        idxKeep2 = data.Observation & data.Compliance;
        data2 = data(idxKeep2,:);
        
        coverage2 = numel(data2.Activity)*epoch;
        [IS2,IV2] = isiv2(data2.DateTime,data2.Activity);
        
        if subject < 60
            shortTerm1.([session,'_',protocol,'_Coverage_Days'])(iShortTerm) = floor(days(coverage1));
            shortTerm1.([session,'_',protocol,'_IS'])(iShortTerm) = IS1;
            shortTerm1.([session,'_',protocol,'_IV'])(iShortTerm) = IV1;
            
            shortTerm2.([session,'_',protocol,'_Coverage_Days'])(iShortTerm) = days(coverage2);
            shortTerm2.([session,'_',protocol,'_IS'])(iShortTerm) = IS2;
            shortTerm2.([session,'_',protocol,'_IV'])(iShortTerm) = IV2;
        elseif subject >= 60
            longTerm1.([session,'_',protocol,'_Coverage_Days'])(iLongTerm) = floor(days(coverage1));
            longTerm1.([session,'_',protocol,'_IS'])(iLongTerm) = IS1;
            longTerm1.([session,'_',protocol,'_IV'])(iLongTerm) = IV1;
            
            longTerm2.([session,'_',protocol,'_Coverage_Days'])(iLongTerm) = days(coverage2);
            longTerm2.([session,'_',protocol,'_IS'])(iLongTerm) = IS2;
            longTerm2.([session,'_',protocol,'_IV'])(iLongTerm) = IV2;
        end
        
    end
    
    if subject < 60
        shortTerm1.subject(iShortTerm) = subject;
        shortTerm2.subject(iShortTerm) = subject;
        iShortTerm = iShortTerm + 1;
    elseif subject >= 60
        longTerm1.subject(iLongTerm) = subject;
        longTerm2.subject(iLongTerm) = subject;
        iLongTerm = iLongTerm + 1;
    end
    
end

%% Save results
writetable(shortTerm1,xlsxPath1,'Sheet','Short Term');
writetable(longTerm1,xlsxPath1,'Sheet','Long Term');

writetable(shortTerm2,xlsxPath2,'Sheet','Short Term');
writetable(longTerm2,xlsxPath2,'Sheet','Long Term');