section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 2
  mov [esp-4], eax
  mov eax, [esp-4]
  mov [esp-8], eax
  mov eax, [esp-8]
  mov [esp-12], eax
  mov eax, [esp-12]
  add eax, 1
  mov [esp-16], eax
  mov eax, [esp-4]
  mov [esp-24], eax
  mov eax, [esp-16]
  mov [esp-20], eax
  mov eax, [esp-20]
  ret
