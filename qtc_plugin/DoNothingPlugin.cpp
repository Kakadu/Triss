#include "DoNothingPlugin.h"
#include <QtPlugin>
#include <QStringList>

#include <coreplugin/coreconstants.h>
#include <coreplugin/actionmanager/actionmanager.h>
#include <coreplugin/actionmanager/command.h>
#include <coreplugin/icore.h>
#include <QKeySequence>
#include <QtGui/QMessageBox>

DoNothingPlugin::DoNothingPlugin()
{
    // Do nothing
}

DoNothingPlugin::~DoNothingPlugin()
{
    // Do notning
}

bool DoNothingPlugin::initialize(const QStringList& args, QString *errMsg)
{
    // The initialize() method is called when Qt Creator wants the plugin to initialize itself. This function is ideally used to initialize the internal state of the plugin and register actions/objects with Qt Creator.
    // The function is called after all the dependencies of this plugin have been loaded.

    Q_UNUSED(args);
    Q_UNUSED(errMsg);

    createMenuItems();
    createMenus();

    // Return true signifying that the initialization was successful.
    // If the initialization was unsuccessful (for some wired reason); the errMsg string should be set to a human readable error message.
    return true;
}

void DoNothingPlugin::extensionsInitialized()
{
    // The extensionsInitialized() method is called after this plugin has been initialized (ie. after initialize() method has been called). This method is called on plugins that depend on this plugin first.

    // Do nothing
}

void DoNothingPlugin::shutdown()
{
    //The shutdown() method is called when the plugin is about to be unloaded.

    // Do nothing
}

void DoNothingPlugin::createMenuItems()
{
    // Fetch the action manager
    Core::ActionManager* am = Core::ICore::instance()->actionManager();

    // Create a command for "About DoNothing"
    Core::Command* cmd = am->registerAction(new QAction(this), "DoNothingPlugin.AboutDoNothingItem", QList<int>() << Core::Constants::C_GLOBAL_ID);
    cmd->action()->setText("About DoNothing");

    // Add the command "Do Nothing" in the beginning of Help menu
    am->actionContainer(Core::Constants::M_HELP)->addAction(cmd);

    // Since menu-items are QActions, we can connect to their triggered(bool) or
    // toggled(bool) signal and respond to trigger/toggled events
    connect(cmd->action(), SIGNAL(triggered(bool)), this, SLOT(about()));

    // Create a command for "About DoNothing 2"
    Core::Command* cmd2 = am->registerAction(new QAction(this), "DoNothingPlugin.AboutDoNothing2Item", QList<int>() << Core::Constants::C_GLOBAL_ID);
    cmd2->action()->setText("About DoNothing 2");

    // Insert the "DoNothing 2" item before "About Plugins..."
    QMenu* helpMenu = am->actionContainer(Core::Constants::M_HELP)->menu();
    QAction* aboutPluginsAction = am->command(Core::Constants::ABOUT_PLUGINS)->action();
    helpMenu->insertAction(aboutPluginsAction, cmd2->action());

    // Connect the action
    connect(cmd2->action(), SIGNAL(triggered(bool)), this, SLOT(about()));
}

void DoNothingPlugin::createMenus()
{
    // Fetch the action manager
    Core::ActionManager* am = Core::ICore::instance()->actionManager();

    // Create a DoNothing menu
    Core::ActionContainer* ac = am->createMenu("DoNothingPlugin.DoNothingMenu");
    ac->menu()->setTitle(tr("DoNothing"));

    // Create a command for "About DoNothing".
    Core::Command* cmd = am->registerAction(new QAction(this), "DoNothingPlugin.AboutDoNothing", QList<int>() << 0);
    cmd->action()->setText("About DoNothing");

    // Add DoNothing menu to the beginning of the menu bar
    am->actionContainer(Core::Constants::MENU_BAR)->addMenu(ac);

    // Add the "About DoNothing" action to the DoNothing menu
    ac->addAction(cmd);

    // Connect the action
    connect(cmd->action(), SIGNAL(triggered(bool)), this, SLOT(about()));

    // Create a DoNothing2 menu
    Core::ActionContainer* ac2 = am->createMenu("DoNothingPlugin.DoNothing2Menu");
    ac2->menu()->setTitle(tr("DoNothing2"));

    // Create a command for "About DoNothing 2".
    Core::Command* cmd2 = am->registerAction(new QAction(this), "DoNothingPlugin.AboutDoNothing2", QList<int>() << 0);
    cmd2->action()->setText("About DoNothing 2");

    // Insert the "DoNothing" menu between "Window" and "Help".
    QMenu* helpMenu = am->actionContainer(Core::Constants::M_HELP)->menu();
    QMenuBar* menuBar = am->actionContainer(Core::Constants::MENU_BAR)->menuBar();
    menuBar->insertMenu(helpMenu->menuAction(), ac2->menu());

    // Add the "About DoNothing 2" action to the DoNothing2 menu
    ac2->addAction(cmd2);

    // Connect the action
    connect(cmd2->action(), SIGNAL(triggered(bool)), this, SLOT(about()));
}

void DoNothingPlugin::about()
{
    QMessageBox::information(0, "About DoNothing Plugin", "Seriously dude, this plugin does nothing", QMessageBox::Ok);
}

// export the plugin class
Q_EXPORT_PLUGIN(DoNothingPlugin)

	
