section .text
global our_code_starts_here
our_code_starts_here:
  mov eax, 55
  mov [esp-4], eax
  mov eax, 1
  mov [esp-8], eax
  mov eax, 2
  mov [esp-12], eax
  mov eax, [esp-12]
  add eax, 1
  mov [esp-16], eax
  mov eax, 3
  mov [esp-20], eax
  mov eax, [esp-20]
  add eax, 1
  mov [esp-24], eax
  mov eax, [esp-8]
  cmp eax, 0
  je if_false_23
  mov eax, [esp-16]
  jmp done_23
  if_false_23:
  mov eax, [esp-24]
  done_23:
  mov [esp-28], eax
  mov eax, 0
  mov [esp-32], eax
  mov eax, 4
  mov [esp-36], eax
  mov eax, [esp-36]
  sub eax, 1
  mov [esp-40], eax
  mov eax, 5
  mov [esp-44], eax
  mov eax, [esp-44]
  sub eax, 1
  mov [esp-48], eax
  mov eax, [esp-32]
  cmp eax, 0
  je if_false_46
  mov eax, [esp-40]
  jmp done_46
  if_false_46:
  mov eax, [esp-48]
  done_46:
  mov [esp-52], eax
  mov eax, [esp-4]
  cmp eax, 0
  je if_false_52
  mov eax, [esp-28]
  jmp done_52
  if_false_52:
  mov eax, [esp-52]
  done_52:
  mov [esp-56], eax
  mov eax, [esp-56]
  mov [esp-60], eax
  mov eax, [esp-60]
  mov [esp-64], eax
  mov eax, [esp-56]
  mov [esp-72], eax
  mov eax, [esp-64]
  mov [esp-68], eax
  mov eax, [esp-68]
  ret
