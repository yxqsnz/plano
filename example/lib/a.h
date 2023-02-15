#pragma once
#include <gtk/gtk.h>

#define HELLO puts("hey!")

void print_hello(GtkWidget *widget, gpointer data);
void activate(GtkApplication *app, gpointer user_data);