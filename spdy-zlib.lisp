(defvar *SPDY_dictionary_txt* #(#x00  #x00  #x00  #x07  #x6f  #x70  #x74  #x69    ; - - - - o p t i
                                #x6f  #x6e  #x73  #x00  #x00  #x00  #x04  #x68    ; o n s - - - - h
                                #x65  #x61  #x64  #x00  #x00  #x00  #x04  #x70    ; e a d - - - - p
                                #x6f  #x73  #x74  #x00  #x00  #x00  #x03  #x70    ; o s t - - - - p
                                #x75  #x74  #x00  #x00  #x00  #x06  #x64  #x65    ; u t - - - - d e
                                #x6c  #x65  #x74  #x65  #x00  #x00  #x00  #x05    ; l e t e - - - -
                                #x74  #x72  #x61  #x63  #x65  #x00  #x00  #x00    ; t r a c e - - -
                                #x06  #x61  #x63  #x63  #x65  #x70  #x74  #x00    ; - a c c e p t -
                                #x00  #x00  #x0e  #x61  #x63  #x63  #x65  #x70    ; - - - a c c e p
                                #x74  #x2d  #x63  #x68  #x61  #x72  #x73  #x65    ; t - c h a r s e
                                #x74  #x00  #x00  #x00  #x0f  #x61  #x63  #x63    ; t - - - - a c c
                                #x65  #x70  #x74  #x2d  #x65  #x6e  #x63  #x6f    ; e p t - e n c o
                                #x64  #x69  #x6e  #x67  #x00  #x00  #x00  #x0f    ; d i n g - - - -
                                #x61  #x63  #x63  #x65  #x70  #x74  #x2d  #x6c    ; a c c e p t - l
                                #x61  #x6e  #x67  #x75  #x61  #x67  #x65  #x00    ; a n g u a g e -
                                #x00  #x00  #x0d  #x61  #x63  #x63  #x65  #x70    ; - - - a c c e p
                                #x74  #x2d  #x72  #x61  #x6e  #x67  #x65  #x73    ; t - r a n g e s
                                #x00  #x00  #x00  #x03  #x61  #x67  #x65  #x00    ; - - - - a g e -
                                #x00  #x00  #x05  #x61  #x6c  #x6c  #x6f  #x77    ; - - - a l l o w
                                #x00  #x00  #x00  #x0d  #x61  #x75  #x74  #x68    ; - - - - a u t h
                                #x6f  #x72  #x69  #x7a  #x61  #x74  #x69  #x6f    ; o r i z a t i o
                                #x6e  #x00  #x00  #x00  #x0d  #x63  #x61  #x63    ; n - - - - c a c
                                #x68  #x65  #x2d  #x63  #x6f  #x6e  #x74  #x72    ; h e - c o n t r
                                #x6f  #x6c  #x00  #x00  #x00  #x0a  #x63  #x6f    ; o l - - - - c o
                                #x6e  #x6e  #x65  #x63  #x74  #x69  #x6f  #x6e    ; n n e c t i o n
                                #x00  #x00  #x00  #x0c  #x63  #x6f  #x6e  #x74    ; - - - - c o n t
                                #x65  #x6e  #x74  #x2d  #x62  #x61  #x73  #x65    ; e n t - b a s e
                                #x00  #x00  #x00  #x10  #x63  #x6f  #x6e  #x74    ; - - - - c o n t
                                #x65  #x6e  #x74  #x2d  #x65  #x6e  #x63  #x6f    ; e n t - e n c o
                                #x64  #x69  #x6e  #x67  #x00  #x00  #x00  #x10    ; d i n g - - - -
                                #x63  #x6f  #x6e  #x74  #x65  #x6e  #x74  #x2d    ; c o n t e n t -
                                #x6c  #x61  #x6e  #x67  #x75  #x61  #x67  #x65    ; l a n g u a g e
                                #x00  #x00  #x00  #x0e  #x63  #x6f  #x6e  #x74    ; - - - - c o n t
                                #x65  #x6e  #x74  #x2d  #x6c  #x65  #x6e  #x67    ; e n t - l e n g
                                #x74  #x68  #x00  #x00  #x00  #x10  #x63  #x6f    ; t h - - - - c o
                                #x6e  #x74  #x65  #x6e  #x74  #x2d  #x6c  #x6f    ; n t e n t - l o
                                #x63  #x61  #x74  #x69  #x6f  #x6e  #x00  #x00    ; c a t i o n - -
                                #x00  #x0b  #x63  #x6f  #x6e  #x74  #x65  #x6e    ; - - c o n t e n
                                #x74  #x2d  #x6d  #x64  #x35  #x00  #x00  #x00    ; t - m d 5 - - -
                                #x0d  #x63  #x6f  #x6e  #x74  #x65  #x6e  #x74    ; - c o n t e n t
                                #x2d  #x72  #x61  #x6e  #x67  #x65  #x00  #x00    ; - r a n g e - -
                                #x00  #x0c  #x63  #x6f  #x6e  #x74  #x65  #x6e    ; - - c o n t e n
                                #x74  #x2d  #x74  #x79  #x70  #x65  #x00  #x00    ; t - t y p e - -
                                #x00  #x04  #x64  #x61  #x74  #x65  #x00  #x00    ; - - d a t e - -
                                #x00  #x04  #x65  #x74  #x61  #x67  #x00  #x00    ; - - e t a g - -
                                #x00  #x06  #x65  #x78  #x70  #x65  #x63  #x74    ; - - e x p e c t
                                #x00  #x00  #x00  #x07  #x65  #x78  #x70  #x69    ; - - - - e x p i
                                #x72  #x65  #x73  #x00  #x00  #x00  #x04  #x66    ; r e s - - - - f
                                #x72  #x6f  #x6d  #x00  #x00  #x00  #x04  #x68    ; r o m - - - - h
                                #x6f  #x73  #x74  #x00  #x00  #x00  #x08  #x69    ; o s t - - - - i
                                #x66  #x2d  #x6d  #x61  #x74  #x63  #x68  #x00    ; f - m a t c h -
                                #x00  #x00  #x11  #x69  #x66  #x2d  #x6d  #x6f    ; - - - i f - m o
                                #x64  #x69  #x66  #x69  #x65  #x64  #x2d  #x73    ; d i f i e d - s
                                #x69  #x6e  #x63  #x65  #x00  #x00  #x00  #x0d    ; i n c e - - - -
                                #x69  #x66  #x2d  #x6e  #x6f  #x6e  #x65  #x2d    ; i f - n o n e -
                                #x6d  #x61  #x74  #x63  #x68  #x00  #x00  #x00    ; m a t c h - - -
                                #x08  #x69  #x66  #x2d  #x72  #x61  #x6e  #x67    ; - i f - r a n g
                                #x65  #x00  #x00  #x00  #x13  #x69  #x66  #x2d    ; e - - - - i f -
                                #x75  #x6e  #x6d  #x6f  #x64  #x69  #x66  #x69    ; u n m o d i f i
                                #x65  #x64  #x2d  #x73  #x69  #x6e  #x63  #x65    ; e d - s i n c e
                                #x00  #x00  #x00  #x0d  #x6c  #x61  #x73  #x74    ; - - - - l a s t
                                #x2d  #x6d  #x6f  #x64  #x69  #x66  #x69  #x65    ; - m o d i f i e
                                #x64  #x00  #x00  #x00  #x08  #x6c  #x6f  #x63    ; d - - - - l o c
                                #x61  #x74  #x69  #x6f  #x6e  #x00  #x00  #x00    ; a t i o n - - -
                                #x0c  #x6d  #x61  #x78  #x2d  #x66  #x6f  #x72    ; - m a x - f o r
                                #x77  #x61  #x72  #x64  #x73  #x00  #x00  #x00    ; w a r d s - - -
                                #x06  #x70  #x72  #x61  #x67  #x6d  #x61  #x00    ; - p r a g m a -
                                #x00  #x00  #x12  #x70  #x72  #x6f  #x78  #x79    ; - - - p r o x y
                                #x2d  #x61  #x75  #x74  #x68  #x65  #x6e  #x74    ; - a u t h e n t
                                #x69  #x63  #x61  #x74  #x65  #x00  #x00  #x00    ; i c a t e - - -
                                #x13  #x70  #x72  #x6f  #x78  #x79  #x2d  #x61    ; - p r o x y - a
                                #x75  #x74  #x68  #x6f  #x72  #x69  #x7a  #x61    ; u t h o r i z a
                                #x74  #x69  #x6f  #x6e  #x00  #x00  #x00  #x05    ; t i o n - - - -
                                #x72  #x61  #x6e  #x67  #x65  #x00  #x00  #x00    ; r a n g e - - -
                                #x07  #x72  #x65  #x66  #x65  #x72  #x65  #x72    ; - r e f e r e r
                                #x00  #x00  #x00  #x0b  #x72  #x65  #x74  #x72    ; - - - - r e t r
                                #x79  #x2d  #x61  #x66  #x74  #x65  #x72  #x00    ; y - a f t e r -
                                #x00  #x00  #x06  #x73  #x65  #x72  #x76  #x65    ; - - - s e r v e
                                #x72  #x00  #x00  #x00  #x02  #x74  #x65  #x00    ; r - - - - t e -
                                #x00  #x00  #x07  #x74  #x72  #x61  #x69  #x6c    ; - - - t r a i l
                                #x65  #x72  #x00  #x00  #x00  #x11  #x74  #x72    ; e r - - - - t r
                                #x61  #x6e  #x73  #x66  #x65  #x72  #x2d  #x65    ; a n s f e r - e
                                #x6e  #x63  #x6f  #x64  #x69  #x6e  #x67  #x00    ; n c o d i n g -
                                #x00  #x00  #x07  #x75  #x70  #x67  #x72  #x61    ; - - - u p g r a
                                #x64  #x65  #x00  #x00  #x00  #x0a  #x75  #x73    ; d e - - - - u s
                                #x65  #x72  #x2d  #x61  #x67  #x65  #x6e  #x74    ; e r - a g e n t
                                #x00  #x00  #x00  #x04  #x76  #x61  #x72  #x79    ; - - - - v a r y
                                #x00  #x00  #x00  #x03  #x76  #x69  #x61  #x00    ; - - - - v i a -
                                #x00  #x00  #x07  #x77  #x61  #x72  #x6e  #x69    ; - - - w a r n i
                                #x6e  #x67  #x00  #x00  #x00  #x10  #x77  #x77    ; n g - - - - w w
                                #x77  #x2d  #x61  #x75  #x74  #x68  #x65  #x6e    ; w - a u t h e n
                                #x74  #x69  #x63  #x61  #x74  #x65  #x00  #x00    ; t i c a t e - -
                                #x00  #x06  #x6d  #x65  #x74  #x68  #x6f  #x64    ; - - m e t h o d
                                #x00  #x00  #x00  #x03  #x67  #x65  #x74  #x00    ; - - - - g e t -
                                #x00  #x00  #x06  #x73  #x74  #x61  #x74  #x75    ; - - - s t a t u
                                #x73  #x00  #x00  #x00  #x06  #x32  #x30  #x30    ; s - - - - 2 0 0
                                #x20  #x4f  #x4b  #x00  #x00  #x00  #x07  #x76    ; - O K - - - - v
                                #x65  #x72  #x73  #x69  #x6f  #x6e  #x00  #x00    ; e r s i o n - -
                                #x00  #x08  #x48  #x54  #x54  #x50  #x2f  #x31    ; - - H T T P - 1
                                #x2e  #x31  #x00  #x00  #x00  #x03  #x75  #x72    ; - 1 - - - - u r
                                #x6c  #x00  #x00  #x00  #x06  #x70  #x75  #x62    ; l - - - - p u b
                                #x6c  #x69  #x63  #x00  #x00  #x00  #x0a  #x73    ; l i c - - - - s
                                #x65  #x74  #x2d  #x63  #x6f  #x6f  #x6b  #x69    ; e t - c o o k i
                                #x65  #x00  #x00  #x00  #x0a  #x6b  #x65  #x65    ; e - - - - k e e
                                #x70  #x2d  #x61  #x6c  #x69  #x76  #x65  #x00    ; p - a l i v e -
                                #x00  #x00  #x06  #x6f  #x72  #x69  #x67  #x69    ; - - - o r i g i
                                #x6e  #x31  #x30  #x30  #x31  #x30  #x31  #x32    ; n 1 0 0 1 0 1 2
                                #x30  #x31  #x32  #x30  #x32  #x32  #x30  #x35    ; 0 1 2 0 2 2 0 5
                                #x32  #x30  #x36  #x33  #x30  #x30  #x33  #x30    ; 2 0 6 3 0 0 3 0
                                #x32  #x33  #x30  #x33  #x33  #x30  #x34  #x33    ; 2 3 0 3 3 0 4 3
                                #x30  #x35  #x33  #x30  #x36  #x33  #x30  #x37    ; 0 5 3 0 6 3 0 7
                                #x34  #x30  #x32  #x34  #x30  #x35  #x34  #x30    ; 4 0 2 4 0 5 4 0
                                #x36  #x34  #x30  #x37  #x34  #x30  #x38  #x34    ; 6 4 0 7 4 0 8 4
                                #x30  #x39  #x34  #x31  #x30  #x34  #x31  #x31    ; 0 9 4 1 0 4 1 1
                                #x34  #x31  #x32  #x34  #x31  #x33  #x34  #x31    ; 4 1 2 4 1 3 4 1
                                #x34  #x34  #x31  #x35  #x34  #x31  #x36  #x34    ; 4 4 1 5 4 1 6 4
                                #x31  #x37  #x35  #x30  #x32  #x35  #x30  #x34    ; 1 7 5 0 2 5 0 4
                                #x35  #x30  #x35  #x32  #x30  #x33  #x20  #x4e    ; 5 0 5 2 0 3 - N
                                #x6f  #x6e  #x2d  #x41  #x75  #x74  #x68  #x6f    ; o n - A u t h o
                                #x72  #x69  #x74  #x61  #x74  #x69  #x76  #x65    ; r i t a t i v e
                                #x20  #x49  #x6e  #x66  #x6f  #x72  #x6d  #x61    ; - I n f o r m a
                                #x74  #x69  #x6f  #x6e  #x32  #x30  #x34  #x20    ; t i o n 2 0 4 -
                                #x4e  #x6f  #x20  #x43  #x6f  #x6e  #x74  #x65    ; N o - C o n t e
                                #x6e  #x74  #x33  #x30  #x31  #x20  #x4d  #x6f    ; n t 3 0 1 - M o
                                #x76  #x65  #x64  #x20  #x50  #x65  #x72  #x6d    ; v e d - P e r m
                                #x61  #x6e  #x65  #x6e  #x74  #x6c  #x79  #x34    ; a n e n t l y 4
                                #x30  #x30  #x20  #x42  #x61  #x64  #x20  #x52    ; 0 0 - B a d - R
                                #x65  #x71  #x75  #x65  #x73  #x74  #x34  #x30    ; e q u e s t 4 0
                                #x31  #x20  #x55  #x6e  #x61  #x75  #x74  #x68    ; 1 - U n a u t h
                                #x6f  #x72  #x69  #x7a  #x65  #x64  #x34  #x30    ; o r i z e d 4 0
                                #x33  #x20  #x46  #x6f  #x72  #x62  #x69  #x64    ; 3 - F o r b i d
                                #x64  #x65  #x6e  #x34  #x30  #x34  #x20  #x4e    ; d e n 4 0 4 - N
                                #x6f  #x74  #x20  #x46  #x6f  #x75  #x6e  #x64    ; o t - F o u n d
                                #x35  #x30  #x30  #x20  #x49  #x6e  #x74  #x65    ; 5 0 0 - I n t e
                                #x72  #x6e  #x61  #x6c  #x20  #x53  #x65  #x72    ; r n a l - S e r
                                #x76  #x65  #x72  #x20  #x45  #x72  #x72  #x6f    ; v e r - E r r o
                                #x72  #x35  #x30  #x31  #x20  #x4e  #x6f  #x74    ; r 5 0 1 - N o t
                                #x20  #x49  #x6d  #x70  #x6c  #x65  #x6d  #x65    ; - I m p l e m e
                                #x6e  #x74  #x65  #x64  #x35  #x30  #x33  #x20    ; n t e d 5 0 3 -
                                #x53  #x65  #x72  #x76  #x69  #x63  #x65  #x20    ; S e r v i c e -
                                #x55  #x6e  #x61  #x76  #x61  #x69  #x6c  #x61    ; U n a v a i l a
                                #x62  #x6c  #x65  #x4a  #x61  #x6e  #x20  #x46    ; b l e J a n - F
                                #x65  #x62  #x20  #x4d  #x61  #x72  #x20  #x41    ; e b - M a r - A
                                #x70  #x72  #x20  #x4d  #x61  #x79  #x20  #x4a    ; p r - M a y - J
                                #x75  #x6e  #x20  #x4a  #x75  #x6c  #x20  #x41    ; u n - J u l - A
                                #x75  #x67  #x20  #x53  #x65  #x70  #x74  #x20    ; u g - S e p t -
                                #x4f  #x63  #x74  #x20  #x4e  #x6f  #x76  #x20    ; O c t - N o v -
                                #x44  #x65  #x63  #x20  #x30  #x30  #x3a  #x30    ; D e c - 0 0 - 0
                                #x30  #x3a  #x30  #x30  #x20  #x4d  #x6f  #x6e    ; 0 - 0 0 - M o n
                                #x2c  #x20  #x54  #x75  #x65  #x2c  #x20  #x57    ; - - T u e - - W
                                #x65  #x64  #x2c  #x20  #x54  #x68  #x75  #x2c    ; e d - - T h u -
                                #x20  #x46  #x72  #x69  #x2c  #x20  #x53  #x61    ; - F r i - - S a
                                #x74  #x2c  #x20  #x53  #x75  #x6e  #x2c  #x20    ; t - - S u n - -
                                #x47  #x4d  #x54  #x63  #x68  #x75  #x6e  #x6b    ; G M T c h u n k
                                #x65  #x64  #x2c  #x74  #x65  #x78  #x74  #x2f    ; e d - t e x t -
                                #x68  #x74  #x6d  #x6c  #x2c  #x69  #x6d  #x61    ; h t m l - i m a
                                #x67  #x65  #x2f  #x70  #x6e  #x67  #x2c  #x69    ; g e - p n g - i
                                #x6d  #x61  #x67  #x65  #x2f  #x6a  #x70  #x67    ; m a g e - j p g
                                #x2c  #x69  #x6d  #x61  #x67  #x65  #x2f  #x67    ; - i m a g e - g
                                #x69  #x66  #x2c  #x61  #x70  #x70  #x6c  #x69    ; i f - a p p l i
                                #x63  #x61  #x74  #x69  #x6f  #x6e  #x2f  #x78    ; c a t i o n - x
                                #x6d  #x6c  #x2c  #x61  #x70  #x70  #x6c  #x69    ; m l - a p p l i
                                #x63  #x61  #x74  #x69  #x6f  #x6e  #x2f  #x78    ; c a t i o n - x
                                #x68  #x74  #x6d  #x6c  #x2b  #x78  #x6d  #x6c    ; h t m l - x m l
                                #x2c  #x74  #x65  #x78  #x74  #x2f  #x70  #x6c    ; - t e x t - p l
                                #x61  #x69  #x6e  #x2c  #x74  #x65  #x78  #x74    ; a i n - t e x t
                                #x2f  #x6a  #x61  #x76  #x61  #x73  #x63  #x72    ; - j a v a s c r
                                #x69  #x70  #x74  #x2c  #x70  #x75  #x62  #x6c    ; i p t - p u b l
                                #x69  #x63  #x70  #x72  #x69  #x76  #x61  #x74    ; i c p r i v a t
                                #x65  #x6d  #x61  #x78  #x2d  #x61  #x67  #x65    ; e m a x - a g e
                                #x3d  #x67  #x7a  #x69  #x70  #x2c  #x64  #x65    ; - g z i p - d e
                                #x66  #x6c  #x61  #x74  #x65  #x2c  #x73  #x64    ; f l a t e - s d
                                #x63  #x68  #x63  #x68  #x61  #x72  #x73  #x65    ; c h c h a r s e
                                #x74  #x3d  #x75  #x74  #x66  #x2d  #x38  #x63    ; t - u t f - 8 c
                                #x68  #x61  #x72  #x73  #x65  #x74  #x3d  #x69    ; h a r s e t - i
                                #x73  #x6f  #x2d  #x38  #x38  #x35  #x39  #x2d    ; s o - 8 8 5 9 -
                                #x31  #x2c  #x75  #x74  #x66  #x2d  #x2c  #x2a    ; 1 - u t f - - -
                                #x2c  #x65  #x6e  #x71  #x3d  #x30  #x2e))          ; - e n q - 0 -
