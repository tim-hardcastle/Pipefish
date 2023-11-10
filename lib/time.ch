import

gocode "time"
gocode "fmt"

def

Time = struct(year, month, day, hour, min, sec, nsec int, loc string)

Duration = struct(hours, mins, secs, nsecs int)

Weekday = enum SUN, MON, TUE, WED, THUR, FRI, SAT

add (t Time, d Duration) :
    Time(goAdd(t[year], t[month], t[day], t[hour], t[min], t[sec], t[nsec], t[loc], d[hours], d[mins], d[secs], d[nsecs]))

goAdd(year, month, day, hour, min, sec, nsec int, loc string, hours, mins, secs, nsecs int) : gocode {
    newTime := (goTime(year, month, day, hour, min, sec, nsec, loc)).Add(charmDurationToGoDuration(hours, mins, secs, nsecs))
    return newTime.Year(), charmMonth(newTime.Month()), newTime.Day(), newTime.Hour(), newTime.Minute(), newTime.Second(), newTime.Nanosecond(), newTime.Location().String()
}

weekday(t Time) :
    Weekday[goWeekday(t[year], t[month], t[day], t[hour], t[min], t[loc])]

goWeekday(year, month, day, hour, min int, loc string) : gocode {
    return int(goTime(year, month, day, hour, min, 0, 0, loc).Weekday())
}

gocode {

    func charmMonth(m time.Month) int {
        return int(m)
    }

    func goTime(year, month, day, hour, min, sec, nsec int, loc string) time.Time {
        locObj, _ := time.LoadLocation(loc)
        var goMonth time.Month
        switch month {
        case 1 :
            goMonth = time.January
        case 2 :
            goMonth = time.February
        case 3 :
            goMonth = time.March
        case 4 :
            goMonth = time.April
        case 5 :
            goMonth = time.May
        case 6 :
            goMonth = time.June
        case 7 :
            goMonth = time.July
        case 8 :
            goMonth = time.August
        case 9 :
            goMonth = time.September
        case 10 :
            goMonth = time.October
        case 11 :
            goMonth = time.November
        case 12 :
            goMonth = time.December
        }
        return time.Date(year, goMonth, day, hour, min, sec, nsec, locObj)
    }

    func charmDurationToGoDuration(hours, mins, secs, nsecs int) time.Duration {
        dur, _ := time.ParseDuration(fmt.Sprintf("%vh%vm%vs%vns", hours, mins, secs, nsecs))
        return dur
    }
}


// func (t Time) Add(d Duration) Time
// func (t Time) AddDate(years int, months int, days int) Time
// func (t Time) After(u Time) bool
// func (t Time) AppendFormat(b []byte, layout string) []byte
// func (t Time) Before(u Time) bool
// func (t Time) Clock() (hour, min, sec int)
// func (t Time) Compare(u Time) int
// func (t Time) Date() (year int, month Month, day int)
// func (t Time) Day() int
// func (t Time) Equal(u Time) bool
// func (t Time) Format(layout string) string
// func (t Time) GoString() string
// func (t *Time) GobDecode(data []byte) error
// func (t Time) GobEncode() ([]byte, error)
// func (t Time) Hour() int
// func (t Time) ISOWeek() (year, week int)
// func (t Time) In(loc *Location) Time
// func (t Time) IsDST() bool
// func (t Time) IsZero() bool
// func (t Time) Local() Time
// func (t Time) Location() *Location
// func (t Time) MarshalBinary() ([]byte, error)
// func (t Time) MarshalJSON() ([]byte, error)
// func (t Time) MarshalText() ([]byte, error)
// func (t Time) Minute() int
// func (t Time) Month() Month
// func (t Time) Nanosecond() int
// func (t Time) Round(d Duration) Time
// func (t Time) Second() int
// func (t Time) String() string
// func (t Time) Sub(u Time) Duration
// func (t Time) Truncate(d Duration) Time
// func (t Time) UTC() Time
// func (t Time) Unix() int64
// func (t Time) UnixMicro() int64
// func (t Time) UnixMilli() int64
// func (t Time) UnixNano() int64
// func (t *Time) UnmarshalBinary(data []byte) error
// func (t *Time) UnmarshalJSON(data []byte) error
// func (t *Time) UnmarshalText(data []byte) error
// func (t Time) Weekday() Weekday
// func (t Time) Year() int
// func (t Time) YearDay() int
// func (t Time) Zone() (name string, offset int)
// func (t Time) ZoneBounds() (start, end Time)