# Python program to demonstrate working of extended
# Euclidean Algorithm

# function for extended Euclidean Algorithm
def gcdExtended(a, b):

    # Base Case
    if a == 0 :
        return 0, 1

    x1, y1 = gcdExtended(b%a, a)

    return (y1 - (b//a) * x1), x1

# Driver code
a, b = 3000,197
x, y = gcdExtended(a, b)
print("(", a, ")", x, "+", "(", b, ")", y, "=", 1)

# (define (solve-ax+by=1 a b)
#   (cond ((= a 0) (cons 0 1))
#         (else
#          (cons
#           (- (cdr (solve-ax+by=1 (modulo b a) a))
#              (* (floor (/ b a)) (car (solve-ax+by=1 (modulo b a) a))))
#           (car (solve-ax+by=1 (modulo b a) a))))))

(define test-public-key1 (key-pair-public test-key-pair1))
(define test-private-key1 (key-pair-private test-key-pair1))
(define result1 (rsa-encrypt "This is a test message." test-public-key1))

(RSA-unconvert-list result1 test-private-key1)

# # Python program to demonstrate working of extended
# # Euclidean Algorithm
#
# # function for extended Euclidean Algorithm
# def gcdExtended(a, b):
#
#     # Base Case
#     if a == 0 :
#         print("Base case reached.\n")
#         return b, 0, 1
#
#     gcd, x1, y1 = gcdExtended(b%a, a)
#
#     # Update x and y using results of recursive
#     # call
#     x = y1 - (b//a) * x1
#     y = x1
#     print("A: ", a, "B: ", b)
#     print("x1: ", x1, "y1: ", y1)
#     print("X: ", x, "Y: ", y)
#     print('')
#
#     return gcd, x, y
#
#
# # Driver code
# a, b = 197,45
# g, x, y = gcdExtended(a, b)
# print("(", a, ")", x, "+", "(", b, ")", y, "=", 1)


(define result2 (encrypt-and-sign "Test message from user 1 to user 2" test-private-key1 test-public-key2))
