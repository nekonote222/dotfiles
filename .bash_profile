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

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi
