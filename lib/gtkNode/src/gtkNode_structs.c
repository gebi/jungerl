#include "gtkNode.h"

void* gn_construct_GtkTextIter() {
  return (void*) g_new0(GtkTextIter,1);}
void* gn_construct_GtkTreeIter() {
  return (void*) g_new0(GtkTreeIter,1);}
void* gn_construct_GtkTreeModelIface() {
  return (void*) g_new0(GtkTreeModelIface,1);}
void* gn_construct_GValue() {
  return (void*) g_new0(GValue,1);}
void* gn_construct_GtkTreePath() {
  return (void*) gtk_tree_path_new();}
void* gn_construct_GdkPoint() {
  return (void*) g_new0(GdkPoint,1);}
void* gn_construct_GdkRectangle() {
  return (void*) g_new0(GdkRectangle,1);}
void* gn_construct_GdkColor() { 
  return (void*) g_new0(GdkColor,1);}
void* gn_construct_PangoLayout() { 
  return (void*) pango_layout_new((PangoContext*) pango_context_new());}
void* gn_construct_PangoContext() {
  return (void*) pango_context_new();}
