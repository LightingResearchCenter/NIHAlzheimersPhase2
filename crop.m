function data = crop(data,varargin)
%CROP Summary of this function goes here
%   Detailed explanation goes here

if nargin == 2
    titleText = varargin{1};
else
    titleText = 'Cropping Actiwatch Data';
end

f = figure;
f.Units = 'normalized';
f.Position = [0,0,1,1];
ax = axes(f);

Activity = data.Activity;
Activity(Activity<1) = 1;

WhiteLight = data.WhiteLight;
WhiteLight(WhiteLight<1) = 1;

logActivity = log10(Activity);
logWhiteLight = log10(WhiteLight);

%% Observation cropping
 loop1 = true;
while loop1
    plot(ax,data.DateTime,[logActivity,logWhiteLight])
    title(ax,titleText,'Interpreter','none')
    legend(ax,'log(Activity)','log(White Light)')
    
    uiwait(msgbox('Select start of observation.','','modal'));
    [x1,~] = zoompick(ax);
    
    uiwait(msgbox('Select end of observation.','','modal'));
    [x2,~] = zoompick(ax);
    
    idx = data.DateTime >= x1 & data.DateTime < x2;
    plot(ax,data.DateTime(idx),[logActivity(idx),logWhiteLight(idx)])
    title(ax,titleText,'Interpreter','none')
    legend(ax,'log(Activity)','log(White Light)')
    
    button = questdlg('Is this selection correct?',...
        'Confirm','Yes','No','Yes');
    if strcmpi(button,'Yes')
        data.Observation = idx;
        loop1 = false;
    end
end

%% Non-compliance cropping
data.Compliance = ~(isnan(data.Activity) | strcmp(data.IntervalStatus,'EXCLUDED'));
hold(ax,'on');
x = data.DateTime(data.Observation);
y = ~data.Compliance(data.Observation);
h = area([x(1);x;x(end)],[0;ax.YLim(2)*y;0]);
h.FaceColor = [0.5 0.5 0.5];
uistack(h,'bottom');
hold(ax,'off')

button = questdlg('Would you like to select subject non-compliance?','','Yes','No','Yes');
if strcmpi(button,'Yes')
    loop1 = true;
    while loop1
        
        loop2 = true;
        while loop2
            
            uiwait(msgbox('Select start of non-compliance.','','modal'));
            [x1,~] = zoompick(ax);
            
            uiwait(msgbox('Select end of non-copmliance.','','modal'));
            [x2,~] = zoompick(ax);
            
            hold(ax,'on');
            y1 = ax.YLim(1);
            y2 = ax.YLim(2);
            h = area([x1,x1,x2,x2],[y1,y2,y2,y1]);
            h.FaceColor = [0.5 0.5 0.5];
            uistack(h,'bottom');
            hold(ax,'off')
            
            button = questdlg('Is this selection correct?','','Yes','No','Yes');
            if strcmpi(button,'Yes')
                idx = data.DateTime >= x1 & data.DateTime < x2;
                data.Compliance = data.Compliance & ~idx;
                loop2 = false;
            else
                delete(h);
            end
        end
        
        button = questdlg('Would you like to select more non-compliance?','','Yes','No','Yes');
        if strcmpi(button,'No')
            loop1 = false;
        end
        
    end
end

close(f)
end


%%
function [x,y] = zoompick(ax)
clc
zoom(ax,'on')
display('Press SPACE BAR to continue');
ax.Parent.CurrentCharacter = 'z';
waitfor(ax.Parent,'CurrentCharacter',char(32));
zoom(ax,'off')
[x,y] = ginput(1);

x = ax.XLim(1) + x*diff(ax.XLim);
y = ax.YLim(1) + y*diff(ax.YLim);

zoom(ax,'out')
clc
end
