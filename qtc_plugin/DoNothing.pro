QTC_SOURCE = /media/disk/kakadu/prog/qt-creator-1.3.1-src/
QTC_BUILD = /media/disk/kakadu/prog/qt-creator-1.3.1-src/build/
TEMPLATE=lib
TARGET=DoNothing
IDE_SOURCE_TREE = $$QTC_SOURCE
IDE_BUILD_TREE = $$QTC_BUILD
PROVIDER = VCreateLogic
DESTDIR = $$QTC_BUILD/lib/qtcreator/plugins/VCreateLogic
LIBS += -L$$QTC_BUILD/lib/qtcreator/plugins/Nokia

include($$QTC_SOURCE/src/qtcreatorplugin.pri)
include($$QTC_SOURCE/src/plugins/coreplugin/coreplugin.pri)
SOURCES  = DoNothingPlugin.cpp
OTHER_FILES = DoNothingPlugin.pluginspec
HEADERS = DoNothingPlugin.h


