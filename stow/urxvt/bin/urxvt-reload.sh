#!/bin/bash

xrdb ~/.Xdefaults && kill -1 $(pidof urxvtd)
