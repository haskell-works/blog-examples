#!/usr/bin/env bash

echo == ./naive.out          ==
time ./naive.out          $1

echo == ./broadword.out      ==
time ./broadword.out      $1

echo == ./simd.out           ==
time ./simd.out           $1

echo == ./naive.auto.out     ==
time ./naive.auto.out     $1

echo == ./broadword.auto.out ==
time ./broadword.auto.out $1

echo == ./simd.auto.out      ==
time ./simd.auto.out      $1

echo == wc -l                ==
time wc -l $1
