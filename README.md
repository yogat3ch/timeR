
# timeR

The goal of timeR is to make it simple to log time on projects with using 
RStudio simple. All that is required is your access token from either platform 
(or both). You can then start and stop the timer for your projects with the 
add in.

## Installation

You can install the latest version of timeR with:

``` r
remotes::install_github("erikor/timeR")
```

## Usage

Click the Addins button in the 
Rstudio toolbar and select "Log Toggl" or "Log Clockify". The interface will 
open in the RStudio viewer, and you can enter your access token (by default, 
this value will be saved in your OS's keychain so you do not need to enter it 
every time). You can then select the appropriate workspace and project, enter a 
description, and start the timer. If the timer is already running, you can 
stop the timer. Click the Done button to continue with your work.

