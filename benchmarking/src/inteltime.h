#ifndef INTELTIME_H
#define INTELTIME_H

#define TIME_DEC \
    uint64_t start_time; \
    uint64_t start_cycles_high; \
    uint64_t start_cycles_low; \
    uint64_t stop_time; \
    uint64_t stop_cycles_high; \
    uint64_t stop_cycles_low; 

#define TIME_START \
    asm volatile ( \
        "cpuid\n\t" \
        "rdtsc\n\t" \
        "mov %%rdx, %0\n\t" \
        "mov %%rax, %1\n\t" \
        : "=r" (start_cycles_high), "=r" (start_cycles_low) \
        :: "%rax", "%rbx", "%rcx", "%rdx"); \

#if defined RDTSCP

#define TIME_STOP \
    asm volatile ( \
        "rdtscp\n\t" \
        "mov %%rdx, %0\n\t" \
        "mov %%rax, %1\n\t" \
        "cpuid\n\t" \
        : "=r" (stop_cycles_high), "=r" (stop_cycles_low) \
        :: "%rax", "%rbx", "%rcx", "%rdx"); \

#elif defined RDTSC

#define TIME_STOP \
    asm volatile ( \
        "cpuid\n\t" \
        "rdtsc\n\t" \
        "mov %%rdx, %0\n\t" \
        "mov %%rax, %1\n\t" \
        : "=r" (stop_cycles_high), "=r"(stop_cycles_low) \
        :: "%rax", "%rbx", "%rcx", "%rdx"); \

#endif

#define TIME_DIFF(diff) \
    start_time = (start_cycles_high << 32) | start_cycles_low; \
    stop_time = (stop_cycles_high << 32) | stop_cycles_low; \
    diff = stop_time - start_time;

#endif
