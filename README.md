# Airtable-API-backup-script-
An R script which backs up all tables of an Airtable base to csv files using an API. 

It is recommended to fill out this script with your API info (obtainable from https://airtable.com/developers/web/api/authentication), then it run of from the terminal of your computer as a cron job, at a time of day when your computer is likely to be turned on. 

IE:
0 12 * * * /usr/local/bin/Rscript "[your Backup.R location]"
When inputted in the cron tab, downlaods your scripts daily at 12pm.

