# java
export _JAVA_OPTIONS="-Dfile.encoding=utf8"

#ant
export _JAVA_OPTIONS="-Dfile.encoding=UTF-8"
export ANT_HOME="/usr/local/ant"
PATH=$PATH:$ANT_HOME/bin
export ANT_OPTS="-Dfile.encoding=UTF-8 -Xmx512m -Xss256k"

##########################
# following export line had been added by egg installer.
# you can delete "Documents/EGGX" directory when you do not use EGGX.
# by mstm. Nov. 2010.
export PATH=$PATH:~/Documents/EGGX/
##########################

export PATH=$PATH:~/bin
export VERSIONER_PYTHON_PREFER_32_BIT=yes

#export PYTHONPATH="/usr/local/lib/python2.7/site-packages/:$PYTHONPATH"
export PYTHONPATH=/usr/local/Cellar/opencv/2.4.5/lib/python2.7/site-packages:$PYTHONPATH

# android
export PATH=$PATH:/Users/hayato/Library/Android/sdk/platform-tools

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi
# added by Anaconda3 2019.10 installer
# >>> conda init >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$(CONDA_REPORT_ERRORS=false '/Users/hayato/opt/anaconda3/bin/conda' shell.bash hook 2> /dev/null)"
if [ $? -eq 0 ]; then
    \eval "$__conda_setup"
else
    if [ -f "/Users/hayato/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/hayato/opt/anaconda3/etc/profile.d/conda.sh"
        CONDA_CHANGEPS1=false conda activate base
    else
        \export PATH="/Users/hayato/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda init <<<
