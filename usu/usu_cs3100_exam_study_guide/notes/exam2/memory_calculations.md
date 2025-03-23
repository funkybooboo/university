## Example 1:

Given:
Process Size = 56,231 bytes
Page Size = 1024 bytes

Find:
Number Of Pages = ceil(Process Size / Page Size) = ceil(56,231 / 1024) = 55 bytes

Total Space = Number Of Pages * Page Size = 55 * 1024 = 56,320 bytes

Internal Fragmentation = Total Space - Process Size = 56,320 - 56,231 = 89 bytes

Number of Bits for Page Number = ceil(log_2(Number Of Pages)) = ceil(log_2(55)) = 6 bits

Offset Bits = log_2(Page Size) = log_2(1024) = 10 bits

Total Bits for Logical Address = Offset Bits + Number of Bits for Page Number = 10 + 6 = 16 bits
