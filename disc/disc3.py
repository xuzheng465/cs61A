def is_prime(n):
    def helper(i):
        if i > (n**0.5):
            return True
        elif n % i == 0:
            return False
        return helper(i + 1)

    return helper(2)

