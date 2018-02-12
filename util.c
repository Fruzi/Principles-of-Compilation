long c_gcd(long a, long b) {
    long c;
    while (a) {
      c = a;
      a = b % a;
      b = c;
    }
    return b < 0 ? (b * -1) : b;
}

long c_divide(long a, long b) {
    return a / b;
}

long c_multiply(long a, long b) {
    return a * b;
}

long c_remainder(long a, long b) {
    return a % b;
}

long c_add_numerator(long an, long ad, long bn, long bd) {
    return an * bd + bn * ad;
}
