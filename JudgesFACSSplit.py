#%%
from datetime import datetime
import time
import os
from moviepy.video.io.ffmpeg_tools import ffmpeg_extract_subclip
import pathlib
import readline

#%%
today = datetime.now()
fldr = "Misc/JudgeSplit_" + today.strftime('%Y%m%d')
os.mkdir(fldr)

#%%
jfacs = ".//Misc//Judges_FACS_v2.mp4"

secs = list(range(0, 370, +10))


#%%
for i in secs:
    print(i)
    start = i
    end = i + 10
    vidname = str(start) + "-" + str(end) + ".mp4"
    #ffmpeg_extract_subclip("", start_time, end_time, targetname="test.mp4")
    ffmpeg_extract_subclip(jfacs, start + 32, end + 32, targetname= "./" + fldr + "/" + vidname)

# %%
