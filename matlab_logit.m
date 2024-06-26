% Specify the path to your CSV file
filePath = 'C:\Users\madou\1_GitHub_Repositories\Stat_Crunch_Competition\Subset_of_Twitch_Streamer_Data_2023.csv';

% Read the CSV file into a table
dataTable = readtable(filePath);

% Display the first few rows of the table
head(dataTable);
%% 

mdl = fitglm(dataTable, 'Followers ~ MeanWeeklyStreamHours', ...
    'Distribution', 'Poisson', 'Link', 'log'); % Use 'Poisson' initially, as 'Negative Binomial' is not directly available

%% 
mdl
%%

% Fit a linear regression model
mdl = fitlm(dataTable, 'Followers ~ MeanWeeklyStreamHours');
%%
% Display the model summary
disp(mdl);
%%
% Plot the data and the fitted regression line
figure;
plot(dataTable.MeanWeeklyStreamHours, dataTable.Followers, 'o'); % Scatter plot of the data
hold on;
plot(dataTable.MeanWeeklyStreamHours, mdl.Fitted, '-r'); % Fitted regression line
xlabel('Mean Weekly Stream Hours');
ylabel('Followers');
title('Simple Linear Regression: Followers vs. Mean Weekly Stream Hours');
legend('Data', 'Fitted Line');
hold off;
%%