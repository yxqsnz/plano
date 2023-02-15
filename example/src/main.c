/* DISCLAIMER: A Major part of this code is from:
 * https://www.gtk.org/docs/getting-started/hello-world/ */
#include "a.h"
#include <gtk/gtk.h>

int main(int argc, char **argv) {
  HELLO;

  GtkApplication *app;
  int status;

  app = gtk_application_new("org.gtk.example", G_APPLICATION_DEFAULT_FLAGS);
  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
  status = g_application_run(G_APPLICATION(app), argc, argv);
  g_object_unref(app);

  return status;
}
