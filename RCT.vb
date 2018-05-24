Module RCT
    Public Function Decompress(strToDecompress As String) As String
        Dim result As String = ""

        Dim counter As Long = 0
        Dim listBoxContains As New ListBox

        For isp = 0 To strToDecompress.Length - 1
            Dim i As String = strToDecompress(isp)
            If i = "[" Then
                Dim counterDebut As Long = counter
                Dim counterLength As Long = 0
                For iss As Long = counter To counter + 9999
                    Dim getChar As String = strToDecompress(iss)
                    Dim getParenthese As String = strToDecompress.Substring(counterDebut, counterLength + 1)
                    If Not getParenthese.EndsWith("]") Then
                        getParenthese = strToDecompress.Substring(counterDebut, counterLength)
                    End If
                    For adding As Long = counterDebut To counterDebut + counterLength
                        listBoxContains.Items.Add(adding)
                    Next
                    If getChar = "]" Then
                        Try
                            Dim parenthèseSplit = getParenthese.Replace("[", "").Replace("]", "").Split("'")
                            Dim parenthèseChar = parenthèseSplit(0)
                            Dim parenthèseAmount = parenthèseSplit(1)
                            For mamadous As Long = 0 To Integer.Parse(parenthèseAmount) - 1
                                result += parenthèseChar
                            Next
                        Catch
                        End Try

                        Exit For
                    End If
                    counterLength += 1
                Next
            End If
            If Not listBoxContains.Items.Contains(isp) Then
                result += i
            End If
            counter += 1
        Next
        Return result
    End Function
    Public Function Compress(strToCompress As String) As String
        Dim result As String = ""

        Dim counterIss As Long = 0
        Dim lastchar As String = strToCompress(0)
        Dim suite As Integer = -1

        For iss = 0 To strToCompress.Length - 1
            counterIss = iss
            Dim i As String = strToCompress(counterIss)
            If strToCompress(counterIss) = lastchar Then
                suite += 1
            Else
                If suite > 5 Then
                    result += "[" & lastchar & "'" & suite + 1 & "]"
                Else
                    For add = 0 To suite
                        result += lastchar
                    Next
                End If
                suite = 0
            End If
            lastchar = strToCompress(counterIss)
        Next
        result += strToCompress(strToCompress.Length - 1)

        Return result
    End Function
End Module