/* -*- c -*- */
#define tort_d_slot(MT,T,N) tort__s(MT) tort__s(N) tort__s(N##SET)
#include "tort/d_slot.h.new"

#define tort_d_mt(MT) tort__s(MT) 
#include "tort/d_mt.h.new"

#define tort_d_m(T,MT,N,F) tort__s(MT) tort__s(N)
#include "tort/d_m.h.new"

