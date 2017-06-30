function autoCrop( varargin )
%AUTOCROP Summary of this function goes here
%   Detailed explanation goes here

%% Enable dependencies
[githubDir,~,~] = fileparts(pwd);
d12packDir      = fullfile(githubDir,'d12pack');
addpath(d12packDir);

%%
if nargin > 0
    dbPathIn = varargin{1};
else
    dbPathIn = '\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\convertedData\partial-crop_2017-06-28_1042.mat';
end

if nargin > 1
    dbPathOut = varargin{2};
else
    timestamp = datestr(now,'yyyy-mm-dd_HHMM');
    dbPathOut = ['\\root\projects\NIH Alzheimers\NIH Alzheimers Phase two study\daysimeterData\convertedData\auto-crop_',timestamp,'.mat'];
end

%%
load(dbPathIn);

%%
[b, a] = butter(5, 0.025, 'low');

for iObj = 1:numel(objArray)
    if all(objArray(iObj).Compliance) % no marked non-compliance
        % Filter activity index
        y = filtfilt(b,a,objArray(iObj).ActivityIndex);
        compliance = y > 0.1;
        noncompliance = ~(compliance | ~objArray(iObj).Observation);
        if any(noncompliance)
            objArray(iObj).Compliance = ~noncompliance;
        end
    end
end

%%
save(dbPathOut,'objArray');


end

