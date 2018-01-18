function actiwatchCosinor
%ACTIWATCHCOSINOR Summary of this function goes here
%   Detailed explanation goes here

% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
circadianDir = fullfile(githubDir,'circadian');
addpath(circadianDir);


% Map paths
timestamp = datestr(now,'yyyy-mm-dd_HHMM');

projectDir = '\\ROOT\projects\NIH Alzheimers\NIH Alzheimers Phase two study\Actiware Data\Actiware files for IV and IS';

ls = dir([projectDir,filesep,'*.mat']);
[~,idxMostRecent] = max(vertcat(ls.datenum));
dataName = ls(idxMostRecent).name;
dataPath = fullfile(projectDir,dataName);

xlsxPath = fullfile(projectDir,'tables',[timestamp,'_Sigmoidfits.xlsx']);


% Import source data
load(dataPath);


% Initialize output
subjects = unique([dataArray.subject]);

nShortTerm = sum(subjects <  60);
nLongTerm  = sum(subjects >= 60);

T = table;


% Perform analysis
iT = 1;
for iD = 1:numel(dataArray)
    thisData = dataArray(iD);
    subject = thisData.subject;
    
    for iSes = 1:numel(thisData)
        session  = thisData(iSes).session;
        protocol = thisData(iSes).protocol;
        group    = thisData(iSes).group;
        
        T.subject(iT) = subject;
        T.session{iT} = session;
        T.protocol{iT} = protocol;
        T.group{iT} = group;
        
        data    = thisData(iSes).data;
        idxKeep = data.Observation & data.Compliance;
        data    = data(idxKeep,:);
        if ~isempty(data)
            % Convert datetime to relative hours from first measurement
            X = hours(data.DateTime - data.DateTime(1));
            % Reassign activity
            y = data.Activity;
            
            cm = sigmoidfit.cos_fit(X,y);
            %             [cos_nlm, cos_fStat, cos_pVal, cm] = sigmoidfit.cos_fit_nlm(X, y);
            %         [alg_nlm, alg_fStat, alg_pVal]     = sigmoidfit.antilog_fit(X, y, cm);
            %         [atn_nlm, atn_fStat, atn_pVal]     = sigmoidfit.arctan_fit( X, y, cm);
            %         [hll_nlm, hll_fStat, hll_pVal]     = sigmoidfit.hill_fit(   X, y, cm);
            
            T.Cos_amplitude(iT) = cm.amp;
            T.Cos_acrophase(iT) = cm.phi;
            T.Cos_rsquared(iT)  = cm.Rsquared;
            
        end % end of if ~isempty(data)
    end % end of for
    
    iT = iT + 1;
end % end of for

T = sortrows(T);

% Save results
writetable(T, xlsxPath);
winopen(xlsxPath);

end



