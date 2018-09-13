%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                             %
%  ACT-R BOLD convolution, using SPMs HRF.                    %
%                                                             %
%  This file is part of:                                      %
%  Borst & Anderson (submitted). A Step-By-Step Tutorial      %
%  on using the Cognitive Architecture ACT-R in combination   % 
%  with fMRI Data. Journal of Mathematical Psychology.        %
%                                                             %
%  Jelmer Borst                                               %
%  University of Groningen                                    %
%  j.p.borst@rug.nl                                           %
%                                                            %
%  150804                                                   %
%                                                          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Change this path to the location of this file:
path_to_this_file = 'location-of-the-tutorial-folder/Tutorial_ACTR_fMRI/ROIanalysis';

%check if path was adapted, otherwise please select the location of this
%file
if strcmp('location-of-the-tutorial-folder/Tutorial_ACTR_fMRI/ROIanalysis',path_to_this_file)
    path_to_this_file = uigetdir(pwd,'Select the location of the current file');
end

%Change directory
try
    cd(path_to_this_file)
catch
    error('Indicated path does not exist; please indicate the location of the current file above.');
end






%%%%%%%%%%%%%%%%%%% Figure 4 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hrf = spm_hrf(.001); %ms HRF kernel


%% Figure 4a, the hemodynamic response function.

plot(0:.001:32,hrf,'color','r') %plot 0-32 seconds
axis([0,32,-.5e-4,2.5e-4]);
set(gca,'XGrid','on');
set(gca,'YGrid','on');
set(gca,'FontSize',14);
set(gca,'FontName','Helvetica');
set(gcf,'Color',[1 1 1]);
xlabel('Time (sec)','FontSize',15);
ylabel('Signal Change (%)','FontSize',15);
title('Hemodynamic Response Function','FontSize',18);


%% Figure 4b, single trial convolution for Retrieval and Manual

% 1. Read in model data, subj 1, block 2, trial 3 (large base, large height)

subj = 1;
bl = 1;
trial = 22;

fname = sprintf('monolingual/model_act_%i_%i_%i.txt',subj,bl,trial);
fid = fopen(fname); %open file
gl = textscan(fid, '%s %f %f'); %read in
modules = gl{1}; %get modules
times = int32(([gl{2} gl{3}]) .* 1000); %get times, converted to ms
fclose(fid); %close file
          
% 2. Convolve

% Make 0-1 demand function
demand_retrieval = zeros(1, 34001); %17 scans / trial = 34 seconds
demand_manual = zeros(1, 34001); %17 scans / trial = 34 seconds

times_retrieval = times(strcmp(modules, "RETRIEVAL"),:); %get times for retrieval
times_manual = times(strcmp(modules, "MANUAL"),:); %get times for manual

% switch demand to 1 for each period of activity
for i = 1:size(times_retrieval,1)
    demand_retrieval(times_retrieval(i,1):times_retrieval(i,2)) = 1;
end
for i = 1:size(times_manual,1)
    demand_manual(times_manual(i,1):times_manual(i,2)) = 1;
end

% Convolve
prediction_retrieval = conv(demand_retrieval,hrf);
prediction_manual = conv(demand_manual,hrf);

% Plot
figure;
subplot(2,1,1);
plot(-4:.001:30,demand_retrieval(1:34001)*.2)
line(-4:.001:30,prediction_retrieval(1:34001),'color','r')
axis([-4,15,-0.2,.8]);
set(gca,'FontSize',14);
set(gca,'FontName','Helvetica');
xlabel('Time (sec)','FontSize',15);
ylabel('Signal Change (%)','FontSize',15);
title('Declarative Memory Predictions','FontSize',18);

subplot(2,1,2);
plot(-4:.001:30,demand_manual(1:34001)*.1)
line(-4:.001:30,prediction_manual(1:34001),'color', 'r')
axis([-4,15,-0.2,.8]);
set(gca,'FontSize',14);
set(gca,'FontName','Helvetica');
xlabel('Time (sec)','FontSize',15);
ylabel('Signal Change (%)','FontSize',15);
title('Manual Predictions','FontSize',18);

set(gcf,'Color',[1 1 1]);

%%






%%%%%%%%%%%%%%%%%%% Figure 5 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%% MAKE SURE THE PATH WAS CHANGED AT THE BEGINNING OF THIS FILE %%%%

try
    cd(path_to_this_file)
catch
    error('Indicated path does not exist; please indicate the location of the current file above.');
end

%%%%%%


%% Figure 5, convolution and aggregation of all trials.
% Monolingual

hrf = spm_hrf(.001); %ms HRF kernel

nsub = 1; %number of subjects
ncond = 3; %number of conditions
info = dlmread('monolingual/actrinfo4mat.txt'); %condition information. Columns: subject, block, trial, condition

topredict = {'RETRIEVAL','IMAGINAL','MANUAL','VISUAL','GOAL','PRODUCTION'};
nmod = length(topredict);

predictions = zeros(34001,ncond,nmod,nsub); % 34 seconds x conditions x modules x subjects

% only 1 subject
subj = 1;

cnt_cond = zeros(3,1); % count the number of trials per condition (averaging on the fly)

% only 1 block
bl = 1;

subinfo = info(info(:,1)==subj & info(:,2) == bl,:); %get all info for this subject & block

for trial = subinfo(:,3)'

    %Read data
    fname = sprintf('monolingual/model_act_%i_%i_%i.txt',subj,bl,trial);
    fid = fopen(fname);
    gl = textscan(fid, '%s %f %f');
    modules = gl{1};
    times = int32(([gl{2} gl{3}]) .* 1000);
    fclose(fid);

    %Create 0-1 demand function for each module ...
    for module = 1:nmod

        tmp = zeros(1, 34001); %17 scans

        tmp2 = times(strcmp(modules, topredict{module}),:);
        tmp2(tmp2 == 0,:) = tmp2(tmp2 == 0,:)+1 ; % add 1 if any value is 0

        for i = 1:size(tmp2,1)
            tmp(tmp2(i,1):tmp2(i,2)) = 1;
        end

        % ... and convolve
        tmp = conv(tmp,hrf);

        %Add to predictions for this subject, condition, and module
        predictions(:,subinfo(trial,4),module,subj) = predictions(:,subinfo(trial,4),module,subj) + tmp(1:34001)';

    end

    cnt_cond(subinfo(trial,4)) = cnt_cond(subinfo(trial,4)) + 1; %Count trials/condition
end
    
%Divide by number of trials in condition (averaging per subj/cond)
for cnd = 1:ncond
    predictions(:,cnd,:,subj) = predictions(:,cnd,:,subj) ./ cnt_cond(cnd);
end

fprintf('Finished monolingual subject %i/%i\n', subj, nsub)

% Average within module/condition monolingual (not necessary for only 1 subject)
predictionsMono = mean(predictions, 4);
% Remove trainings condition
predictionsMono(:,1,:,:) = [];

%% Bilingual
info = dlmread('bilingual/actrinfo4mat.txt'); %condition information. Columns: subject, block, trial, condition

predictions = zeros(34001,ncond,nmod,nsub); % 34 seconds x conditions x modules x subjects

cnt_cond = zeros(3,1); % count the number of trials per condition (averaging on the fly)

subinfo = info(info(:,1)==subj & info(:,2) == bl,:); %get all info for this subject & block

for trial = subinfo(:,3)'

    %Read data
    fname = sprintf('bilingual/model_act_%i_%i_%i.txt',subj,bl,trial);
    fid = fopen(fname);
    gl = textscan(fid, '%s %f %f');
    modules = gl{1};
    times = int32(([gl{2} gl{3}]) .* 1000);
    fclose(fid);

    %Create 0-1 demand function for each module ...
    for module = 1:nmod

        tmp = zeros(1, 34001); %17 scans

        tmp2 = times(strcmp(modules, topredict{module}),:);
        tmp2(tmp2 == 0,:) = tmp2(tmp2 == 0,:)+1 ; % add 1 if any value is 0

        for i = 1:size(tmp2,1)
            tmp(tmp2(i,1):tmp2(i,2)) = 1;
        end

        % ... and convolve
        tmp = conv(tmp,hrf);

        %Add to predictions for this subject, condition, and module
        predictions(:,subinfo(trial,4),module,subj) = predictions(:,subinfo(trial,4),module,subj) + tmp(1:34001)';

    end

    cnt_cond(subinfo(trial,4)) = cnt_cond(subinfo(trial,4)) + 1; %Count trials/condition
end
    
%Divide by number of trials in condition (averaging per subj/cond)
for cnd = 1:ncond
    predictions(:,cnd,:,subj) = predictions(:,cnd,:,subj) ./ cnt_cond(cnd);
end

fprintf('Finished bilingual subject %i/%i\n', subj, nsub)

%Average within module/condition bilingual
predictionsBi = mean(predictions, 4);
% Remove trainings condition
predictionsBi(:,1,:,:) = [];

%% Merge monolingual and bilingual

predictions = cat(2,predictionsMono,predictionsBi);
ncond = 4; % 4 conditions: monoPrac, monoNov,BiPrac,BiNov

%% Plot

figure('Position',[10,10,1000,800]);
dblue = [31 120 180] / 255;
dorange = [255 127 0] / 255;
colors = [dblue;dblue;dorange;dorange];
markers = {'*','d','*','d'};
titles = {'Declarative Memory', 'Problem State', 'Manual', 'Visual', 'Goal','Production'};
for mod = 1:nmod

    subplot(3,2,mod);
    plot(NaN)
    axis([-4,20,min(min(predictions(:,:,mod)))*2,max(max(predictions(:,:,mod)))*1.1]);

    line([0 0],[-1 1],'LineWidth', .75,'Color',[.7,.7,.7],'LineStyle', ':')
    line([-4 30],[0 0],'LineWidth', .75,'Color',[.7,.7,.7],'LineStyle', ':')
    
    lines = [];
    for cond = 1:ncond
        lines(cond) = line(-3:2:27,predictions(1000:2000:31000,cond,mod),'color', colors(cond,:),'Marker',markers{cond},'MarkerSize',10);
    end
    set(gca,'FontSize',18);
    set(gca,'FontName','Helvetica');
    xlabel('Time (sec)','FontSize',19);
    ylabel('Signal Change (%)','FontSize',19);
    title(titles{mod},'FontSize',22);
end

legend(lines,{'Monolingual $ Practiced', 'Monolingual $ Novel', 'Bilingual $ Practiced', 'Bilingual $ Novel'},'Box','off')
set(gcf,'Color',[1 1 1]);




